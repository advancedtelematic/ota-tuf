 package com.advancedtelematic.tuf.reposerver.http

import java.time.Instant
import java.time.temporal.ChronoUnit

import cats.implicits._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, SignedPayload}
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, TargetItem}
import com.advancedtelematic.tuf.reposerver.db.{SignedRoleRepositorySupport, TargetItemRepositorySupport}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepository.SignedRoleNotFound

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}


class SignedRoleGeneration(keyserverClient: KeyserverClient)
                          (implicit val db: Database, val ec: ExecutionContext) extends SignedRoleRepositorySupport {

  val targetRoleGeneration = new TargetRoleGeneration(keyserverClient)

  def regenerateSignedRoles(repoId: RepoId): Future[SignedPayload[Json]] = {
    async {
      val expireAt = defaultExpire

      val signedRoot = await(fetchAndCacheRootRole(repoId))

      val targetVersion = await(nextVersion(repoId, RoleType.TARGETS))
      val targetRole = await(targetRoleGeneration.generate(repoId, expireAt, targetVersion))
      val signedTarget = await(signRole(repoId, RoleType.TARGETS, targetRole))

      val snapshotVersion = await(nextVersion(repoId, RoleType.SNAPSHOT))
      val snapshotRole = genSnapshotRole(signedRoot, signedTarget, expireAt, snapshotVersion)
      val signedSnapshot = await(signRole(repoId, RoleType.SNAPSHOT, snapshotRole))

      val timestampVersion = await(nextVersion(repoId, RoleType.TIMESTAMP))
      val timestampRole = genTimestampRole(signedSnapshot, expireAt, timestampVersion)
      val signedTimestamp = await(signRole(repoId, RoleType.TIMESTAMP, timestampRole))

      await(signedRoleRepo.persistAll(signedTarget, signedSnapshot, signedTimestamp))

      signedTarget.content
    }
  }

  def addToTarget(targetItem: TargetItem): Future[SignedPayload[Json]] =
    targetRoleGeneration.addTargetItem(targetItem).flatMap(_ ⇒ regenerateSignedRoles(targetItem.repoId))

  def signRole[T <: VersionedRole : Decoder : Encoder](repoId: RepoId, roleType: RoleType, role: T): Future[SignedRole] = {
    keyserverClient.sign(repoId, roleType, role).map { signedRole =>
      SignedRole.withChecksum(repoId, roleType, signedRole, role.version)
    }
  }

  def fetchAndCacheRootRole(repoId: RepoId): Future[SignedRole] = {
    signedRoleRepo.find(repoId, RoleType.ROOT).recoverWith {
      case SignedRoleNotFound =>
        keyserverClient.fetchRootRole(repoId).flatMap { rootRole =>
          val signedRoot = SignedRole.withChecksum(repoId, RoleType.ROOT, rootRole, version = rootRole.signed.version)
          signedRoleRepo.persist(signedRoot)
        }
    }
  }

  private def nextVersion(repoId: RepoId, roleType: RoleType): Future[Int] = {
    signedRoleRepo
      .find(repoId, roleType)
      .map { signedRole =>
        signedRole
          .content
          .asJson
          .hcursor
          .downField("signed")
          .downField("version")
          .as[Int]
          .getOrElse(0) + 1
      }
      .recover {
        case SignedRoleNotFound => 1
      }
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
  extends TargetItemRepositorySupport {

  def addTargetItem(targetItem: TargetItem): Future[TargetItem] =
    targetItemRepo.persist(targetItem)

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

