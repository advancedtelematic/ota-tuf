 package com.advancedtelematic.keyserver.roles

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.advancedtelematic.keyserver.data.KeyServerCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.keyserver.data.ClientDataType._
import com.advancedtelematic.libtuf.repo_store.RoleKeyStoreClient
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.keyserver.data.RepositoryDataType._
import com.advancedtelematic.keyserver.db.{SignedRoleRepositorySupport, TargetItemRepositorySupport}
import io.circe.syntax._
import io.circe.{Encoder, Json}
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.keyserver.db.SignedRoleRepository.SignedRoleNotFound

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}

class SignedRoleGeneration(roleSigningClient: RoleKeyStoreClient)
                          (implicit val db: Database, val ec: ExecutionContext)
extends SignedRoleRepositorySupport {

  val targetRoleGeneration = new TargetRoleGeneration(roleSigningClient)

  def addToTarget(targetItem: TargetItem): Future[Json] = {
    val repoId = targetItem.repoId

    async {
      val expireAt = defaultExpire

      val signedRoot = await(fetchRootRole(repoId))

      val targetVersion = await(nextVersion(repoId, RoleType.TARGETS))
      val targetRole = await(targetRoleGeneration.updateRoleWith(targetItem, expireAt, targetVersion))
      val signedTarget = await(signRole(repoId, RoleType.TARGETS, targetRole))

      val snapshotVersion = await(nextVersion(repoId, RoleType.SNAPSHOT))
      val snapshotRole = genSnapshotRole(signedRoot, signedTarget, expireAt, snapshotVersion)
      val signedSnapshot = await(signRole(repoId, RoleType.SNAPSHOT, snapshotRole))

      val timestampVersion = await(nextVersion(repoId, RoleType.TIMESTAMP))
      val timestampRole = genTimestampRole(signedSnapshot, expireAt, timestampVersion)
      val signedTimestamp = await(signRole(repoId, RoleType.TIMESTAMP, timestampRole))

      val persistF = signedRoleRepo.persistAll(signedTarget, signedSnapshot, signedTimestamp)
      await(persistF)

      signedTarget.content
    }
  }

  def signRole[T <: VersionedRole : Encoder](repoId: RepoId, roleType: RoleType, role: T): Future[SignedRole] = {
    roleSigningClient.sign(repoId, roleType, role).map { jsonRole =>
      SignedRole.withChecksum(repoId, roleType, jsonRole.asJson, role.version)
    }
  }

  def fetchRootRole(repoId: RepoId): Future[SignedRole] = {
    roleSigningClient.fetchRootRole(repoId).flatMap { rootRoleJson =>
      val signedRoot = SignedRole.withChecksum(repoId, RoleType.ROOT, rootRoleJson.asJson, version = 1)
      signedRoleRepo.persist(signedRoot)
    }
  }

  private def nextVersion(repoId: RepoId, roleType: RoleType): Future[Int] = {
    signedRoleRepo
      .find(repoId, roleType)
      .map { signedRole =>
        signedRole
          .content
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

protected class TargetRoleGeneration(roleSigningClient: RoleKeyStoreClient)
                          (implicit val db: Database, val ec: ExecutionContext)
  extends TargetItemRepositorySupport {

  def updateRoleWith(targetItem: TargetItem, expireAt: Instant, version: Int): Future[TargetsRole] = {
    targetItemRepo
      .persist(targetItem)
      .flatMap(_ => generate(targetItem.repoId, expireAt, version))
  }

  private def generate(repoId: RepoId, expireAt: Instant, version: Int): Future[TargetsRole] = {
    targetItemRepo.findFor(repoId).map { targetItems =>
      val targets = targetItems.map { item =>
        val hashes = Map(item.checksum.method -> item.checksum.hash)
        item.filename -> ClientTargetItem(hashes, item.length)
      }.toMap

      TargetsRole(expireAt, targets, version)
    }
  }
}
