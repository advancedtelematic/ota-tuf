package com.advancedtelematic.tuf.reposerver.http

import java.time.Instant
import java.time.temporal.ChronoUnit

import akka.http.scaladsl.util.FastFuture
import cats.syntax.either._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{JsonSignedPayload, RepoId, RoleType, TargetFilename}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, SignedRoleNotDbLOL, TargetItem}
import com.advancedtelematic.tuf.reposerver.db.FilenameCommentRepository
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepository.SignedRoleNotFound
import com.advancedtelematic.tuf.reposerver.db.{SignedRoleRepository, SignedRoleRepositorySupport, TargetItemRepositorySupport}
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import slick.jdbc.MySQLProfile.api._

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}
import cats.syntax.either._
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import org.slf4j.LoggerFactory
import shapeless._

// TODO:SM How to not use SinedRole all the time, if we need the signed version ?
// Create separate SignedRole[T] with the same attributes but not linked to db ?
// Do we already have that?
// THERE IS A MIX OF EVERYTHING

private class RoleSigner {
  def apply[T : TufRole](v: T): SignedRoleNotDbLOL[T] = ???
}

object RoleRefresh {
  type SignFn[T : TufRole] = T => SignedRoleNotDbLOL[T]
}

class RoleRefresh(signFn: RoleSigner) {
    def refreshTargets(existing: TargetsRole, snapshotRole: SnapshotRole, timestampRole: TimestampRole): (SignedRoleNotDbLOL[TargetsRole], SignedRoleNotDbLOL[SnapshotRole], TimestampRole) = {
    val newTargetsRole = refreshRole(existing)
    val signedTargets = signFn(newTargetsRole)
    val (snapshotRole, timestampRole) = refreshSnapshots(snapshotRole, signedTargets, timestampRole)
    (signedTargets, snapshotRole, timestampRole)
  }

  // TODO:SM Extract generate to other site, use same code
  def refreshSnapshots(existing: SignedRoleNotDbLOL[SnapshotRole], targetsRole: SignedRoleNotDbLOL[TargetsRole], timestampRole: TimestampRole): (SignedRoleNotDbLOL[SnapshotRole], TimestampRole) = {
    val refreshed = refreshRole(existing)

    val newMeta: Map[MetaPath, MetaItem] = existing.meta + targetsRole.asMetaRole

    val newSnapshot = SnapshotRole(newMeta, refreshed.expiresAt, refreshed.version)

    val signedSnapshot = signFn(newSnapshot)

    val timestampRole = refreshTimestamp(signedSnapshot, timestampRole)

    (signedSnapshot, timestampRole)
  }

  def refreshTimestamp(snapshotRole: SignedRoleNotDbLOL[SnapshotRole], timestampRole: TimestampRole): TimestampRole = {
    val refreshed = refreshRole(timestampRole)
    TimestampRole(Map(snapshotRole.asMetaRole), refreshed.expires, refreshed.version)
  }

  private def refreshRole[T](existing: T)(implicit tufRole: TufRole[T]): T = {
    val nextExpires = Instant.now.plus(1, ChronoUnit.DAYS)
    tufRole.refreshRole(existing, _ + 1, nextExpires)
  }
}

// TODO:SM How to use this?
abstract class RoleGeneration {
  def generateTargets: Future[(TargetsRole, SnapshotRole, TimestampRole)]

  def generateSnapshots(): Future[(SnapshotRole, TimestampRole)]

  def generateTimestamp(): Future[TimestampRole]
}

class SignedRoleGeneration(keyserverClient: KeyserverClient)
                          (implicit val db: Database, val ec: ExecutionContext) extends SignedRoleRepositorySupport {

  private val log = LoggerFactory.getLogger(this.getClass)

  val targetRoleGeneration = new TargetRoleGeneration(keyserverClient)

  def regenerateAllSignedRoles(repoId: RepoId): Future[JsonSignedPayload] = {
    async {
      await(fetchRootRole(repoId))

      val expireAt = defaultExpire

      val targetVersion = await(nextVersion(repoId, RoleType.TARGETS))
      val targetRole = await(targetRoleGeneration.generate(repoId, expireAt, targetVersion))
      val signedTarget = await(signRole(repoId, targetRole))

      val dependent = await(regenerateSignedDependent(repoId, signedTarget, expireAt))

      await(signedRoleRepo.persistAll(signedTarget :: dependent))

      signedTarget.content
    }
  }

  def regenerateSignedDependent(repoId: RepoId, targetRole: SignedRole, expireAt: Instant): Future[List[SignedRole]] = async {
    val signedRoot = await(fetchRootRole(repoId))

    val snapshotVersion = await(nextVersion(repoId, RoleType.SNAPSHOT))
    val snapshotRole = genSnapshotRole(signedRoot, targetRole, expireAt, snapshotVersion)
    val signedSnapshot = await(signRole(repoId, snapshotRole))

    val timestampVersion = await(nextVersion(repoId, RoleType.TIMESTAMP))
    val timestampRole = genTimestampRole(signedSnapshot, expireAt, timestampVersion)
    val signedTimestamp = await(signRole(repoId, timestampRole))

    List(signedSnapshot, signedTimestamp)
  }

  def addTargetItem(targetItem: TargetItem): Future[JsonSignedPayload] =
    targetRoleGeneration.addTargetItem(targetItem).flatMap(_ ⇒ regenerateAllSignedRoles(targetItem.repoId))

  def deleteTargetItem(repoId: RepoId, filename: TargetFilename): Future[Unit] = for {
    _ <- ensureTargetsCanBeSigned(repoId)
    _ <- targetRoleGeneration.deleteTargetItem(repoId, filename)
    _ <- regenerateAllSignedRoles(repoId)
  } yield ()

  def signRole[T <: VersionedRole : TufRole : Decoder : Encoder](repoId: RepoId, role: T): Future[SignedRole] = {
    keyserverClient.sign(repoId, role.roleType, role.asJson).map { signedRole =>
      SignedRole.withChecksum(repoId, role.roleType, signedRole, role.version, role.expires)
    }
  }

  private def ensureTargetsCanBeSigned(repoId: RepoId): Future[SignedRole] = async {
    val rootRole = await(signedRoleRepo.find(repoId, RoleType.TARGETS)).content.signed.as[TargetsRole].valueOr(throw _)
    await(signRole(repoId, rootRole))
  }

  private def fetchRootRole(repoId: RepoId): Future[SignedRole] =
    keyserverClient.fetchRootRole(repoId).map { rootRole =>
      SignedRole.withChecksum(repoId, RoleType.ROOT, rootRole.asJsonSignedPayload, rootRole.signed.version, rootRole.signed.expires)
    }

  private def findAndCacheRole(repoId: RepoId, roleType: RoleType): Future[SignedRole] = {
    signedRoleRepo
      .find(repoId, roleType)
      .recoverWith { case SignedRoleNotFound => generateAndCacheRole(repoId, roleType) }
  }

  private def generateRoleAndDependenciesZZZZ(): Future[SignedRole] = {
    val versionedRole = role.content.signed.as[T].valueOr(throw _)
    val nextVersion = versionedRole.version + 1
    val nextExpires = Instant.now.plus(1, ChronoUnit.DAYS)
    val newRole = updateRoleFn(versionedRole, nextExpires, nextVersion)

    signRole(repoId, newRole).flatMap(sr => signedRoleRepo.persist(sr))
  }

  private def findFreshRole[T <: VersionedRole : TufRole : Decoder : Encoder](repoId: RepoId, roleType: RoleType, refreshFn: => T): Future[SignedRole] = {
    signedRoleRepo.find(repoId, roleType).flatMap { role =>
      val futureRole =
        if (role.expireAt.isBefore(Instant.now.plus(1, ChronoUnit.HOURS))) {
          FastFuture.successful(refreshFn())
        } else
          FastFuture.successful(role)

      futureRole.recoverWith {
        case KeyserverClient.RoleKeyNotFound =>
          log.info(s"Could not update $roleType (for $repoId) because the keys are missing, returning expired version")
          FastFuture.successful(role)
      }
    }.recoverWith {
      case SignedRoleRepository.SignedRoleNotFound =>
        generateAndCacheRole(repoId, roleType)
    }
  }

  private def generateAndCacheRole(repoId: RepoId, roleType: RoleType): Future[SignedRole] = {
    regenerateAllSignedRoles(repoId)
      .recoverWith { case err => log.warn("Could not generate signed roles", err) ; FastFuture.failed(SignedRoleNotFound) }
      .flatMap(_ => signedRoleRepo.find(repoId, roleType))
  }

  val refresher = new RoleRefresh(???)

  def findRole(repoId: RepoId, roleType: RoleType): Future[SignedRole] = {
    roleType match {
      case RoleType.ROOT =>
        fetchRootRole(repoId)
      case r @ RoleType.SNAPSHOT =>



        findFreshRole[SnapshotRole](repoId, r, refresher.refreshSnapshots())
      case r @ RoleType.TIMESTAMP =>
        findFreshRole[TimestampRole](repoId, r)
      case r @ RoleType.TARGETS =>
        findFreshRole[TargetsRole](repoId, r)
      case _ =>
        findAndCacheRole(repoId, roleType)
    }
  }

  private def nextVersion(repoId: RepoId, roleType: RoleType): Future[Int] =
    signedRoleRepo
      .find(repoId, roleType)
      .map(_.version + 1)
      .recover {
        case SignedRoleNotFound => 1
      }

  private def genSnapshotRole(root: SignedRole, target: SignedRole, expireAt: Instant, version: Int): SnapshotRole = {
    val meta = List(root.asMetaRole, target.asMetaRole).toMap
    SnapshotRole(meta, expireAt, version)
  }

  private def genTimestampRole(snapshotRole: SignedRole, expireAt: Instant, version: Int): TimestampRole = {
    val meta = Map(snapshotRole.asMetaRole)
    TimestampRole(meta, expireAt, version)
  }

  private def defaultExpire: Instant =
    Instant.now().plus(31, ChronoUnit.DAYS)
}

protected class TargetRoleGeneration(roleSigningClient: KeyserverClient)
                          (implicit val db: Database, val ec: ExecutionContext)
  extends TargetItemRepositorySupport with FilenameCommentRepository.Support {

  def addTargetItem(targetItem: TargetItem): Future[TargetItem] =
    targetItemRepo.persist(targetItem)

  def deleteTargetItem(repoId: RepoId, filename: TargetFilename): Future[Unit] =
    targetItemRepo.deleteItemAndComments(filenameCommentRepo)(repoId, filename)

  def generate(repoId: RepoId, expireAt: Instant, version: Int): Future[TargetsRole] = {
    targetItemRepo.findFor(repoId).map { targetItems =>
      val targets = targetItems.map { item =>
        val hashes = Map(item.checksum.method -> item.checksum.hash)
        item.filename -> ClientTargetItem(hashes, item.length, item.custom.map(_.asJson))
      }.toMap

      TargetsRole(expireAt, targets, version)
    }
  }
}

