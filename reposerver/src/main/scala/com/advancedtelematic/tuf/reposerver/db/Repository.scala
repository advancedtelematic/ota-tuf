package com.advancedtelematic.tuf.reposerver.db

import java.time.Instant
import scala.util.Success
import scala.util.Failure
import akka.NotUsed
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.util.FastFuture
import akka.stream.scaladsl.Source
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libats.data.{ErrorCode, PaginationResult}
import com.advancedtelematic.libats.http.Errors.{EntityAlreadyExists, MissingEntity, MissingEntityId, RawError}
import com.advancedtelematic.libtuf.data.TufDataType.{JsonSignedPayload, RepoId, RoleType, TargetFilename}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType._
import com.advancedtelematic.libtuf_server.repo.server.DataType._
import com.advancedtelematic.libats.slick.db.SlickExtensions._
import com.advancedtelematic.libats.slick.codecs.SlickRefined._
import com.advancedtelematic.libats.slick.db.SlickUUIDKey._
import com.advancedtelematic.libats.slick.db.SlickAnyVal._
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, DelegatedRoleName, SnapshotRole, TimestampRole, TufRole}
import com.advancedtelematic.libtuf_server.data.Requests.TargetComment
import com.advancedtelematic.libtuf_server.data.TufSlickMappings._
import com.advancedtelematic.tuf.reposerver.db.DBDataType.{DbDelegation, DbSignedRole}
import com.advancedtelematic.tuf.reposerver.db.TargetItemRepositorySupport.MissingNamespaceException
import com.advancedtelematic.tuf.reposerver.http.Errors._
import com.advancedtelematic.libtuf_server.repo.server.Errors.SignedRoleNotFound
import shapeless.ops.function.FnToProduct
import shapeless.{Generic, HList, Succ}
import SlickValidatedString._
import akka.actor.Scheduler
import com.advancedtelematic.libats.slick.db.DatabaseHelper.DatabaseWithRetry
import com.advancedtelematic.libtuf_server.repo.server.SignedRoleProvider
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.TargetItem

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace
import slick.jdbc.MySQLProfile.api._
import slick.lifted.AbstractTable

trait DatabaseSupport {
  implicit val ec: ExecutionContext
  implicit val db: Database
  implicit val scheduler: Scheduler
}

object TargetItemRepositorySupport {
  case class MissingNamespaceException(repoId: RepoId) extends Exception(s"Unknown namespace for repo $repoId") with NoStackTrace
}

trait TargetItemRepositorySupport extends DatabaseSupport {
  lazy val targetItemRepo = new TargetItemRepository()
}

protected [db] class TargetItemRepository()(implicit db: Database, ec: ExecutionContext, scheduler: Scheduler) {
  import Schema.targetItems

  def persist(targetItem: TargetItem): Future[TargetItem] = db.runWithRetry(persistAction(targetItem))

  def deleteItemAndComments(filenameComments: FilenameCommentRepository)(repoId: RepoId, filename: TargetFilename): Future[Unit] = db.runWithRetry {
    val deleteItemAction = targetItems.filter(_.repoId === repoId).filter(_.filename === filename).delete
    filenameComments.deleteAction(repoId, filename)
      .andThen(deleteItemAction)
      .map(_ => ()).transactionally
  }

  def deleteItemsAndComments(filenameComments: FilenameCommentRepository)(repoId: RepoId, filenames: Set[TargetFilename]): Future[Unit] = db.runWithRetry {
    val deleteItemsAction = targetItems.filter(_.repoId === repoId).filter(_.filename inSet filenames).delete
    filenameComments.deleteAction(repoId, filenames)
      .andThen(deleteItemsAction)
      .map(_ => ()).transactionally
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

  def findFor(repoId: RepoId): Future[Seq[TargetItem]] = db.runWithRetry {
    targetItems.filter(_.repoId === repoId).result
  }

  def exists(repoId: RepoId, filename: TargetFilename): Future[Boolean] = {
    findByFilename(repoId, filename)
      .transform {
        case Success(_) => Success(true)
        case Failure(TargetNotFoundError) => Success(false)
        case Failure(err) => Failure(err)
      }
  }

  def findByFilename(repoId: RepoId, filename: TargetFilename): Future[TargetItem] = db.runWithRetry {
    targetItems
      .filter(_.repoId === repoId)
      .filter(_.filename === filename)
      .result
      .failIfNotSingle(TargetNotFoundError)
  }

  def usage(repoId: RepoId): Future[(Namespace, Long)] =
    db.runWithRetry {
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

protected [db] class SignedRoleRepository()(implicit val db: Database, val ec: ExecutionContext, val scheduler: Scheduler) {
  import DBDataType._

  private val signedRoleRepository = new DbSignedRoleRepository()

  def persistAll(repoId: RepoId, signedRoles: List[SignedRole[_]]): Future[Seq[DbSignedRole]] =
    signedRoleRepository.persistAll(signedRoles.map(_.asDbSignedRole(repoId)))

  def persist[T : TufRole](repoId: RepoId, signedRole: SignedRole[T], forceVersion: Boolean = false): Future[DbSignedRole] =
    signedRoleRepository.persist(signedRole.asDbSignedRole(repoId), forceVersion)

  def find[T](repoId: RepoId)(implicit tufRole: TufRole[T]): Future[SignedRole[T]] =
    signedRoleRepository.find(repoId, tufRole.roleType).map(_.asSignedRole)

  def storeAll(targetItemRepo: TargetItemRepository)(repoId: RepoId, signedRoles: List[SignedRole[_]], items: Seq[TargetItem]): Future[Unit] =
    signedRoleRepository.storeAll(targetItemRepo)(repoId, signedRoles.map(_.asDbSignedRole(repoId)), items)
}


object DbSignedRoleRepository {
  def InvalidVersionBumpError(oldVersion: Int, newVersion: Int) =
    RawError(ErrorCode("invalid_version_bump"), StatusCodes.Conflict, s"Cannot bump version from $oldVersion to $newVersion")
}

protected[db] class DbSignedRoleRepository()(implicit val db: Database, val ec: ExecutionContext, val scheduler: Scheduler) {
  import DbSignedRoleRepository.InvalidVersionBumpError
  import Schema.signedRoles

  import shapeless._

  import DBDataType._

  def persist(signedRole: DbSignedRole, forceVersion: Boolean = false): Future[DbSignedRole] =
    db.runWithRetry(persistAction(signedRole, forceVersion).transactionally)

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

  def persistAll(signedRoles: List[DbSignedRole]): Future[Seq[DbSignedRole]] = db.runWithRetry {
    DBIO.sequence(signedRoles.map(sr => persistAction(sr, forceVersion = false))).transactionally
  }

  def find(repoId: RepoId, roleType: RoleType): Future[DbSignedRole] =
    db.runWithRetry {
      signedRoles
        .filter(_.repoId === repoId)
        .filter(_.roleType === roleType)
        .result
        .headOption
        .failIfNone(SignedRoleNotFound(repoId, roleType))
    }

  def storeAll(targetItemRepo: TargetItemRepository)(repoId: RepoId, signedRoles: List[DbSignedRole], items: Seq[TargetItem]): Future[Unit] = db.runWithRetry {
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

protected[db] class RepoNamespaceRepository()(implicit db: Database, ec: ExecutionContext, scheduler: Scheduler) {
  import com.advancedtelematic.libats.slick.db.SlickPipeToUnit.pipeToUnit
  import Schema.repoNamespaces

  val MissingRepoNamespace = MissingEntity[(RepoId, Namespace)]()

  val AlreadyExists = EntityAlreadyExists[(RepoId, Namespace)]()

  def persist(repoId: RepoId, namespace: Namespace): Future[Unit] = db.runWithRetry {
    (repoNamespaces += RepoNamespace(repoId, namespace)).handleIntegrityErrors(AlreadyExists)
  }

  def ensureNotExists(namespace: Namespace): Future[Unit] =
    findFor(namespace)
      .flatMap(_ => FastFuture.failed(AlreadyExists))
      .recover { case MissingRepoNamespace => () }

  def findFor(namespace: Namespace): Future[RepoId] = db.runWithRetry {
    repoNamespaces
      .filter(_.namespace === namespace)
      .map(_.repoId)
      .result
      .headOption
      .failIfNone(MissingRepoNamespace)
  }

  def belongsTo(repoId: RepoId, namespace: Namespace): Future[Boolean] = db.runWithRetry {
    repoNamespaces
      .filter(_.repoId === repoId)
      .filter(_.namespace === namespace)
      .size
      .result
      .map(_ > 0)
  }

  def list(offset: Long, limit: Long): Future[PaginationResult[RepoNamespace]] = db.runWithRetry {
    repoNamespaces.paginateResult(offset, limit)
  }
}

object FilenameCommentRepository {
  trait Support extends DatabaseSupport {
    lazy val filenameCommentRepo = new FilenameCommentRepository()
  }

  val CommentNotFound = MissingEntity[TargetComment]()

  val PackageMissing = MissingEntity[TargetFilename]()
}

protected [db] class FilenameCommentRepository()(implicit db: Database, ec: ExecutionContext, scheduler: Scheduler) {
  import FilenameCommentRepository._
  import Schema.filenameComments

  def persist(repoId: RepoId, filename: TargetFilename, comment: TargetComment): Future[Int] = db.runWithRetry {
    filenameComments.insertOrUpdate((repoId, filename, comment))
      .handleIntegrityErrors(PackageMissing)
  }

  def find(repoId: RepoId, filename: TargetFilename): Future[TargetComment] = db.runWithRetry {
    filenameComments
      .filter(_.repoId === repoId)
      .filter(_.filename === filename)
      .map(_.comment)
      .result
      .headOption
      .failIfNone(CommentNotFound)
  }

  def find(repoId: RepoId): Future[Seq[(TargetFilename, TargetComment)]] = db.runWithRetry {
    filenameComments
      .filter(_.repoId === repoId)
      .map(filenameComment => (filenameComment.filename, filenameComment.comment))
      .result
  }

  def deleteAction(repoId: RepoId, filename: TargetFilename): DBIO[Int] =
    filenameComments.filter(_.repoId === repoId).filter(_.filename === filename).delete

  def deleteAction(repoId: RepoId, filenames: Set[TargetFilename]): DBIO[Int] =
    filenameComments.filter(_.repoId === repoId).filter(_.filename inSet filenames).delete
}

trait DelegationRepositorySupport extends DatabaseSupport {
  lazy val delegationsRepo = new DelegationRepository()
}

protected [db] class DelegationRepository()(implicit db: Database, ec: ExecutionContext, scheduler: Scheduler) {

  def find(repoId: RepoId, roleNames: DelegatedRoleName*): Future[DbDelegation] = db.runWithRetry {
    Schema.delegations.filter(_.repoId === repoId).filter(_.roleName.inSet(roleNames)).result.failIfNotSingle(DelegationNotFound)
  }

  def persist(repoId: RepoId, roleName: DelegatedRoleName, content: JsonSignedPayload): Future[Unit] = db.runWithRetry {
    Schema.delegations.insertOrUpdate(DbDelegation(repoId, roleName, content)).map(_ => ())
  }
}
