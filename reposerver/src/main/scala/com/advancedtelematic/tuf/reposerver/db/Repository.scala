package com.advancedtelematic.tuf.reposerver.db

import akka.http.scaladsl.model.StatusCodes
import com.advancedtelematic.libats.http.ErrorCode
import com.advancedtelematic.libats.http.Errors.{MissingEntity, RawError}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, TargetItem}
import slick.driver.MySQLDriver.api._

import scala.concurrent.{ExecutionContext, Future}

trait DatabaseSupport {
  implicit val ec: ExecutionContext
  implicit val db: Database
}

trait TargetItemRepositorySupport extends DatabaseSupport {
  lazy val targetItemRepo = new TargetItemRepository()
}

protected [db] class TargetItemRepository()(implicit db: Database, ec: ExecutionContext) {
  import com.advancedtelematic.libats.db.SlickExtensions._

  import Schema.targetItems

  def persist(targetItem: TargetItem): Future[TargetItem] =
    db.run(targetItems.insertOrUpdate(targetItem).map(_ => targetItem))

  def findFor(repoId: RepoId): Future[Seq[TargetItem]] =
    db.run {
      targetItems.filter(_.repoId === repoId).result
    }
}

trait SignedRoleRepositorySupport extends DatabaseSupport {
  lazy val signedRoleRepo = new SignedRoleRepository()
}

object SignedRoleRepository {
  val SignedRoleNotFound = MissingEntity(classOf[SignedRole])
  def InvalidVersionBumpError(oldVersion: Int, newVersion: Int) =
    RawError(ErrorCode("invalid_version_bump"), StatusCodes.Conflict, s"Cannot bump version from $oldVersion to $newVersion")
}

protected[db] class SignedRoleRepository()(implicit db: Database, ec: ExecutionContext) {
  import com.advancedtelematic.libats.db.SlickExtensions._
  import SignedRoleRepository.InvalidVersionBumpError

  import Schema.signedRoles
  import SignedRoleRepository.SignedRoleNotFound

  def persist(signedRole: SignedRole): Future[SignedRole] =
    db.run(persistAction(signedRole))

  private def persistAction(signedRole: SignedRole): DBIO[SignedRole] = {
    signedRoles
      .filter(_.repoId === signedRole.repoId)
      .filter(_.roleType === signedRole.roleType)
      .result
      .headOption
      .flatMap(ensureVersionBumpIsValid(signedRole))
      .flatMap(_ => signedRoles.insertOrUpdate(signedRole))
      .map(_ => signedRole)
  }

  def persistAll(signedRoles: SignedRole*): Future[Seq[SignedRole]] =
    db.run {
      DBIO.sequence(signedRoles.map(persistAction)).transactionally
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

  private def ensureVersionBumpIsValid(signedRole: SignedRole)(oldSignedRole: Option[SignedRole]): DBIO[Unit] =
    oldSignedRole match {
      case Some(sr) if signedRole.roleType != RoleType.ROOT && sr.version != signedRole.version - 1 =>
        DBIO.failed(InvalidVersionBumpError(sr.version, signedRole.version))
      case _ => DBIO.successful(())
    }
}
