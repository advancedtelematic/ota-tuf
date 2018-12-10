package com.advancedtelematic.tuf.reposerver.db

import java.time.Instant

import akka.NotUsed
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.util.FastFuture
import akka.stream.scaladsl.Source
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libats.data.ErrorCode
import com.advancedtelematic.libats.http.Errors.{EntityAlreadyExists, MissingEntity, RawError}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, TargetFilename}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, TargetItem}
import com.advancedtelematic.libats.slick.db.SlickExtensions._
import com.advancedtelematic.libats.slick.codecs.SlickRefined._
import com.advancedtelematic.libats.slick.db.SlickUUIDKey._
import com.advancedtelematic.libats.slick.db.SlickAnyVal._
import com.advancedtelematic.libtuf.data.ClientDataType.{DelegatedRoleName, TufRole}
import com.advancedtelematic.libtuf_server.data.Requests.TargetComment
import com.advancedtelematic.libtuf_server.data.TufSlickMappings._
import com.advancedtelematic.tuf.reposerver.db.DBDataType.{DbDelegation, DbSignedRole}
import com.advancedtelematic.tuf.reposerver.db.TargetItemRepositorySupport.MissingNamespaceException
import com.advancedtelematic.tuf.reposerver.http.Errors
import shapeless.ops.function.FnToProduct
import shapeless.{Generic, HList}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace
import slick.jdbc.MySQLProfile.api._
import slick.lifted.AbstractTable

import scala.concurrent.java8.FuturesConvertersImpl.P


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

  def deleteItemAndComments(filenameComments: FilenameCommentRepository)(repoId: RepoId, filename: TargetFilename): Future[Unit] = db.run {
    filenameComments.deleteAction(repoId, filename)
      .andThen {
        targetItems
          .filter(_.repoId === repoId).filter(_.filename === filename)
          .delete
      }
      .map(_ => ())
      .transactionally
  }

  protected [db] def resetAction(repoId: RepoId): DBIO[Unit] =
    targetItems.filter(_.repoId === repoId).delete.map(_ => ())

  protected [db] def createAction(targetItem: TargetItem): DBIO[TargetItem] =
    (targetItems += targetItem).map(_ => targetItem)

  protected [db] def persistAction(targetItem: TargetItem): DBIO[TargetItem] = {
    val findQ = targetItems.filter(_.repoId === targetItem.repoId).filter(_.filename === targetItem.filename)
    val now = Instant.now

    findQ.result.headOption.flatMap {
      case Some(existing) =>
        val targetCustom = targetItem.custom.map(_.copy(updatedAt = now, createdAt = existing.custom.map(_.createdAt).getOrElse(now)))
        val newTargetItem = targetItem.copy(custom = targetCustom)
        findQ.update(newTargetItem).map(_ => newTargetItem)
      case None =>
        createAction(targetItem.copy(custom = targetItem.custom.map(_.copy(updatedAt = now, createdAt = now))))
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
  lazy val signedRoleRepository = new SignedRoleRepository()
}

protected [db] class SignedRoleRepository()(implicit val db: Database, val ec: ExecutionContext) {
  import DBDataType._

  private val signedRoleRepository = new DbSignedRoleRepository()

  def persistAll(signedRoles: List[SignedRole[_]]): Future[Seq[DbSignedRole]] =
    signedRoleRepository.persistAll(signedRoles.map(_.asDbSignedRole))

  def persist[T : TufRole](signedRole: SignedRole[T], forceVersion: Boolean = false): Future[DbSignedRole] =
    signedRoleRepository.persist(signedRole.asDbSignedRole, forceVersion)

  def find[T](repoId: RepoId)(implicit tufRole: TufRole[T]): Future[SignedRole[T]] =
    signedRoleRepository.find(repoId, tufRole.roleType).map(_.asSignedRole)

  def storeAll(targetItemRepo: TargetItemRepository)(repoId: RepoId, signedRoles: List[SignedRole[_]], items: Seq[TargetItem]): Future[Unit] =
    signedRoleRepository.storeAll(targetItemRepo)(repoId, signedRoles.map(_.asDbSignedRole), items)
}


object DbSignedRoleRepository {
  val SignedRoleNotFound = MissingEntity[DbSignedRole]()
  def InvalidVersionBumpError(oldVersion: Int, newVersion: Int) =
    RawError(ErrorCode("invalid_version_bump"), StatusCodes.Conflict, s"Cannot bump version from $oldVersion to $newVersion")
}

protected[db] class DbSignedRoleRepository()(implicit val db: Database, val ec: ExecutionContext) {
  import DbSignedRoleRepository.InvalidVersionBumpError
  import Schema.signedRoles
  import DbSignedRoleRepository.SignedRoleNotFound

  import shapeless._

  import DBDataType._

  def persist(signedRole: DbSignedRole, forceVersion: Boolean = false): Future[DbSignedRole] =
    db.run(persistAction(signedRole, forceVersion))

  protected [db] def persistAction(signedRole: DbSignedRole, forceVersion: Boolean): DBIO[DbSignedRole] = {
    signedRoles
      .filter(_.repoId === signedRole.repoId)
      .filter(_.roleType === signedRole.roleType)
      .result
      .headOption
      .flatMap { old =>
        if(!forceVersion)
          ensureVersionBumpIsValid(signedRole)(old)
        else
          DBIO.successful(())
      }
      .flatMap(_ => signedRoles.insertOrUpdate(signedRole))
      .map(_ => signedRole)
  }

  def persistAll(signedRoles: List[DbSignedRole]): Future[Seq[DbSignedRole]] = db.run {
    DBIO.sequence(signedRoles.map(sr => persistAction(sr, forceVersion = false))).transactionally
  }

  def find(repoId: RepoId, roleType: RoleType): Future[DbSignedRole] =
    db.run {
      signedRoles
        .filter(_.repoId === repoId)
        .filter(_.roleType === roleType)
        .result
        .headOption
        .failIfNone(SignedRoleNotFound)
    }

  def storeAll(targetItemRepo: TargetItemRepository)(repoId: RepoId, signedRoles: List[DbSignedRole], items: Seq[TargetItem]): Future[Unit] = db.run {
    targetItemRepo.resetAction(repoId)
      .andThen(DBIO.sequence(signedRoles.map(sr => persistAction(sr, forceVersion = false))))
      .andThen(DBIO.sequence(items.map(targetItemRepo.createAction)))
      .map(_ => ())
      .transactionally
  }

  private def ensureVersionBumpIsValid(signedRole: DbSignedRole)(oldSignedRole: Option[DbSignedRole]): DBIO[Unit] =
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

object FilenameCommentRepository {
  trait Support extends DatabaseSupport {
    lazy val filenameCommentRepo = new FilenameCommentRepository()
  }

  val CommentNotFound = MissingEntity[TargetComment]()

  val PackageMissing = MissingEntity[TargetFilename]()
}

protected [db] class FilenameCommentRepository()(implicit db: Database, ec: ExecutionContext) {
  import FilenameCommentRepository._
  import Schema.filenameComments

  def persist(repoId: RepoId, filename: TargetFilename, comment: TargetComment): Future[Int] = db.run {
    filenameComments.insertOrUpdate((repoId, filename, comment))
      .handleIntegrityErrors(PackageMissing)
  }

  def find(repoId: RepoId, filename: TargetFilename): Future[TargetComment] = db.run {
    filenameComments
      .filter(_.repoId === repoId)
      .filter(_.filename === filename)
      .map(_.comment)
      .result
      .headOption
      .failIfNone(CommentNotFound)
  }

  def find(repoId: RepoId): Future[Seq[(TargetFilename, TargetComment)]] = db.run {
    filenameComments
      .filter(_.repoId === repoId)
      .map(filenameComment => (filenameComment.filename, filenameComment.comment))
      .result
      .failIfEmpty(CommentNotFound)
  }

  def deleteAction(repoId: RepoId, filename: TargetFilename) =
    filenameComments.filter(_.repoId === repoId).filter(_.filename === filename)
      .delete
}

trait DelegationRepositorySupport extends DatabaseSupport {
  lazy val delegationsRepo = new DelegationRepository()
}

protected [db] class DelegationRepository()(implicit db: Database, ec: ExecutionContext) {

  def find(repoId: RepoId, roleName: DelegatedRoleName): Future[DbDelegation] = db.run {
    Schema.delegations.filter(_.repoId === repoId).filter(_.roleName === roleName).result.failIfNotSingle(Errors.DelegationNotFound)
  }

  def persist[P <: Product, H <: HList](args: P)(implicit
                                                 gen0: Generic.Aux[P, H],
                                                 gen1: Generic.Aux[DbDelegation, H]): Future[DbDelegation] = db.run {
    persistAction(gen1.from(gen0.to(args)))
  }

  protected [db] def persistAction(value: DbDelegation): DBIO[DbDelegation] =
    (Schema.delegations += value).map(_ => value)
}
