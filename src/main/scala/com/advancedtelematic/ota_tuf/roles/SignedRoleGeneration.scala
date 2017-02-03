package com.advancedtelematic.ota_tuf.roles

import java.time.Instant
import java.time.temporal.ChronoUnit
import com.advancedtelematic.ota_tuf.data.Codecs._
import com.advancedtelematic.ota_tuf.data.DataType.RepoId
import com.advancedtelematic.ota_tuf.data.RepoClientDataType._
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.{SignedRole, TargetItem}
import com.advancedtelematic.ota_tuf.data.RoleType
import com.advancedtelematic.ota_tuf.data.RoleType.RoleType
import com.advancedtelematic.ota_tuf.db.{SignedRoleRepositorySupport, TargetItemRepositorySupport}
import com.advancedtelematic.ota_tuf.repo_store.RoleKeyStoreClient
import io.circe.syntax._
import io.circe.{Encoder, Json}
import slick.driver.MySQLDriver.api._

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

      val targetRole = await(targetRoleGeneration.updateRoleWith(targetItem, expireAt))
      val signedTarget = await(signRole(repoId, RoleType.TARGETS, targetRole))

      val snapshotRole = genSnapshotRole(signedRoot, signedTarget, expireAt)
      val signedSnapshot = await(signRole(repoId, RoleType.SNAPSHOT, snapshotRole))

      val timestampRole = genTimestampRole(signedSnapshot, expireAt)
      val signedTimestamp = await(signRole(repoId, RoleType.TIMESTAMP, timestampRole))

      val persistF = signedRoleRepo.persistAll(signedTarget, signedSnapshot, signedTimestamp)
      await(persistF)

      signedTarget.content
    }
  }

  def signRole[T : Encoder](repoId: RepoId, roleType: RoleType, role: T): Future[SignedRole] = {
    roleSigningClient.sign(repoId, roleType, role).map { jsonRole =>
      SignedRole.withChecksum(repoId, roleType, jsonRole.asJson)
    }
  }

  def fetchRootRole(repoId: RepoId): Future[SignedRole] = {
    roleSigningClient.fetchRootRole(repoId).flatMap { rootRoleJson =>
      val signedRoot = SignedRole.withChecksum(repoId, RoleType.ROOT, rootRoleJson.asJson)
      signedRoleRepo.persist(signedRoot)
    }
  }

  private def genSnapshotRole(root: SignedRole, target: SignedRole, expireAt: Instant): SnapshotRole = {
    val meta = List(root.asMetaRole, target.asMetaRole).toMap
    SnapshotRole(meta, expireAt, version = 1)
  }

  private def genTimestampRole(snapshotRole: SignedRole, expireAt: Instant): TimestampRole = {
    val meta = Map(snapshotRole.asMetaRole)
    TimestampRole(meta, expireAt, version = 1)
  }

  private def defaultExpire: Instant =
    Instant.now().plus(31, ChronoUnit.DAYS)
}

protected class TargetRoleGeneration(roleSigningClient: RoleKeyStoreClient)
                          (implicit val db: Database, val ec: ExecutionContext)
  extends TargetItemRepositorySupport {

  def updateRoleWith(targetItem: TargetItem, expireAt: Instant): Future[TargetsRole] = {
    targetItemRepo
      .persist(targetItem)
      .flatMap(_ => generate(targetItem.repoId, expireAt))
  }

  private def generate(repoId: RepoId, expireAt: Instant): Future[TargetsRole] = {
    targetItemRepo.findFor(repoId).map { targetItems =>
      val targets = targetItems.map { item =>
        val hashes = Map(item.checksum.method -> item.checksum.hash)
        item.filename -> ClientTargetItem(hashes, item.length)
      }.toMap

      TargetsRole(expireAt, targets, version = 1)
    }
  }
}
