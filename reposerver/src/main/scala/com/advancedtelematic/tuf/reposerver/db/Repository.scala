package com.advancedtelematic.tuf.reposerver.db

import java.time.Instant

import akka.NotUsed
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.util.FastFuture
import akka.stream.scaladsl.Source
import com.advancedtelematic.libats.data.Namespace
import com.advancedtelematic.libats.http.ErrorCode
import com.advancedtelematic.libats.http.Errors.{EntityAlreadyExists, MissingEntity, RawError}
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, TargetItem}
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libats.slick.db.SlickExtensions._
import com.advancedtelematic.libats.slick.codecs.SlickRefined._
import com.advancedtelematic.libats.slick.db.SlickUUIDKey._
import com.advancedtelematic.libats.slick.db.SlickAnyVal._
import com.advancedtelematic.tuf.reposerver.db.TargetItemRepositorySupport.MissingNamespaceException
import com.advancedtelematic.tuf.reposerver.http.Errors

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace

trait DatabaseSupport {
  implicit val ec: ExecutionContext
  implicit val db: Database
}

object TargetItemRepositorySupport {
  case class MissingNamespaceException(repoId: RepoId) extends Exception(s"Unknown namespace for repo $repoId") with NoStackTrace
}

trait TargetItemRepositorySupport extends DatabaseSupport {
  lazy val targetItemRepo = new TargetItemRepository()
}

protected [db] class TargetItemRepository()(implicit db: Database, ec: ExecutionContext) {
  import Schema.targetItems

  def persist(targetItem: TargetItem): Future[TargetItem] = db.run(persistAction(targetItem))

  protected [db] def persistAction(targetItem: TargetItem): DBIO[TargetItem] = {
    val findQ = targetItems.filter(_.repoId === targetItem.repoId).filter(_.filename === targetItem.filename)
    val now = Instant.now

    findQ.result.headOption.flatMap {
      case Some(existing) =>
        val targetCustom = targetItem.custom.map(_.copy(updatedAt = now, createdAt = existing.custom.map(_.createdAt).getOrElse(now)))
        val newTargetItem = targetItem.copy(custom = targetCustom)
        findQ.update(newTargetItem).map(_ => newTargetItem)
      case None =>
        (targetItems += targetItem.copy(custom = targetItem.custom.map(_.copy(createdAt = now)))).map(_ => targetItem)
    }.transactionally
  }

  def findFor(repoId: RepoId): Future[Seq[TargetItem]] = db.run {
    targetItems.filter(_.repoId === repoId).result
  }

  def findByFilename(repoId: RepoId, filename: TargetFilename): Future[TargetItem] = db.run {
    targetItems
      .filter(_.repoId === repoId)
      .filter(_.filename === filename)
      .result
      .failIfNotSingle(Errors.TargetNotFoundError)
  }

  def usage(repoId: RepoId): Future[(Namespace, Long)] =
    db.run {
      val usage = targetItems
        .filter(_.repoId === repoId)
        .map(_.length)
        .sum
        .getOrElse(0l)
        .result

      val ns =
        Schema.repoNamespaces
          .filter(_.repoId === repoId)
          .map(_.namespace)
          .result
          .failIfNotSingle(MissingNamespaceException(repoId))

      ns.zip(usage)
    }
}

trait SignedRoleRepositorySupport extends DatabaseSupport {
  lazy val signedRoleRepo = new SignedRoleRepository()
}

object SignedRoleRepository {
  val SignedRoleNotFound = MissingEntity[SignedRole]()
  def InvalidVersionBumpError(oldVersion: Int, newVersion: Int) =
    RawError(ErrorCode("invalid_version_bump"), StatusCodes.Conflict, s"Cannot bump version from $oldVersion to $newVersion")
}

protected[db] class SignedRoleRepository()(implicit val db: Database, val ec: ExecutionContext) {
  import SignedRoleRepository.InvalidVersionBumpError
  import Schema.signedRoles
  import SignedRoleRepository.SignedRoleNotFound

  def persist(signedRole: SignedRole): Future[SignedRole] =
    db.run(persistAction(signedRole))

  protected [db] def persistAction(signedRole: SignedRole): DBIO[SignedRole] = {
    signedRoles
      .filter(_.repoId === signedRole.repoId)
      .filter(_.roleType === signedRole.roleType)
      .result
      .headOption
      .flatMap(ensureVersionBumpIsValid(signedRole))
      .flatMap(_ => signedRoles.insertOrUpdate(signedRole))
      .map(_ => signedRole)
  }

  def persistAll(signedRoles: List[SignedRole]): Future[Seq[SignedRole]] = db.run {
    DBIO.sequence(signedRoles.map(persistAction)).transactionally
  }

  def update(signedRole: SignedRole): Future[Int] =
    db.run {
      signedRoles
        .filter(_.repoId === signedRole.repoId)
        .filter(_.roleType === signedRole.roleType)
        .update(signedRole)
    }



  def findAll(roleType: RoleType): Source[SignedRole, NotUsed] =
    Source.fromPublisher {
      db.stream {
        signedRoles
          .filter(_.roleType === roleType)
          .result
      }
    }

  def find(repoId: RepoId, roleType: RoleType): Future[SignedRole] =
    db.run {
      signedRoles
        .filter(_.repoId === repoId)
        .filter(_.roleType === roleType)
        .result
        .headOption
        .failIfNone(SignedRoleNotFound)
    }

  def storeAll(targetItemRepo: TargetItemRepository)(signedRoles: List[SignedRole], items: Seq[TargetItem]): Future[Unit] = db.run {
    DBIO.sequence(signedRoles.map(persistAction))
      .andThen(DBIO.sequence(items.map(targetItemRepo.persistAction)))
      .map(_ => ())
      .transactionally
  }

  private def ensureVersionBumpIsValid(signedRole: SignedRole)(oldSignedRole: Option[SignedRole]): DBIO[Unit] =
    oldSignedRole match {
      case Some(sr) if signedRole.roleType != RoleType.ROOT && sr.version != signedRole.version - 1 =>
        DBIO.failed(InvalidVersionBumpError(sr.version, signedRole.version))
      case _ => DBIO.successful(())
    }
}

trait RepoNamespaceRepositorySupport extends DatabaseSupport {
  lazy val repoNamespaceRepo = new RepoNamespaceRepository()
}

protected[db] class RepoNamespaceRepository()(implicit db: Database, ec: ExecutionContext) {
  import com.advancedtelematic.libats.slick.db.SlickPipeToUnit.pipeToUnit
  import Schema.repoNamespaces

  val MissingRepoNamespace = MissingEntity[(RepoId, Namespace)]()

  val AlreadyExists = EntityAlreadyExists[(RepoId, Namespace)]()

  def persist(repoId: RepoId, namespace: Namespace): Future[Unit] = db.run {
    (repoNamespaces += (repoId, namespace)).handleIntegrityErrors(AlreadyExists)
  }

  def ensureNotExists(namespace: Namespace): Future[Unit] =
    findFor(namespace)
      .flatMap(_ => FastFuture.failed(AlreadyExists))
      .recover { case MissingRepoNamespace => () }

  def findFor(namespace: Namespace): Future[RepoId] = db.run {
    repoNamespaces
      .filter(_.namespace === namespace)
      .map(_.repoId)
      .result
      .headOption
      .failIfNone(MissingRepoNamespace)
  }

  def belongsTo(repoId: RepoId, namespace: Namespace): Future[Boolean] = db.run {
    repoNamespaces
      .filter(_.repoId === repoId)
      .filter(_.namespace === namespace)
      .size
      .result
      .map(_ > 0)
  }
}
