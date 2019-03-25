package com.advancedtelematic.tuf.reposerver.http

import java.time.Instant
import java.time.temporal.ChronoUnit

import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{JsonSignedPayload, RepoId, RoleType, TargetFilename}
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, TargetItem}
import com.advancedtelematic.tuf.reposerver.db.DbSignedRoleRepository.SignedRoleNotFound
import com.advancedtelematic.tuf.reposerver.db._
import com.advancedtelematic.tuf.reposerver.delegations.SignedRoleDelegationsFind
import io.circe.Encoder
import io.circe.syntax._
import org.slf4j.LoggerFactory
import slick.jdbc.MySQLProfile.api._

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}

class SignedRoleGeneration(keyserverClient: KeyserverClient)
                          (implicit val db: Database, val ec: ExecutionContext)
  extends SignedRoleRepositorySupport with DelegationRepositorySupport {

  private val log = LoggerFactory.getLogger(this.getClass)

  val targetRoleGeneration = new TargetRoleGeneration(keyserverClient)
  val signedRoleDelegationsFind = new SignedRoleDelegationsFind()

  def regenerateAllSignedRoles(repoId: RepoId): Future[JsonSignedPayload] = async {

    await(fetchRootRole(repoId))

    val expireAt = defaultExpire

    val targetVersion = await(nextVersion[TargetsRole](repoId))
    val targetDelegations = await(findTargetRoleDelegations(repoId))
    val targetRole = await(targetRoleGeneration.generate(repoId, targetDelegations, expireAt, targetVersion))
    val signedTarget = await(signRole(repoId, targetRole))

    val (newSnapshots, newTimestamps) = await(freshSignedDependent(repoId, signedTarget, expireAt))

    await(signedRoleRepository.persistAll(List(signedTarget, newSnapshots, newTimestamps)))

    signedTarget.content
  }

  def regenerateSnapshots(repoId: RepoId): Future[(SignedRole[SnapshotRole], SignedRole[TimestampRole])] = async {
    val existingTargets = await(signedRoleRepository.find[TargetsRole](repoId))
    val (snapshots, timestamps) = await(freshSignedDependent(repoId, existingTargets, defaultExpire))
    await(signedRoleRepository.persistAll(List(snapshots, timestamps)))
    (snapshots, timestamps)
  }

  def freshSignedDependent(repoId: RepoId,
                           targetRole: SignedRole[TargetsRole],
                           expireAt: Instant): Future[(SignedRole[SnapshotRole], SignedRole[TimestampRole])] = async {
    val signedRoot = await(fetchRootRole(repoId))

    val snapshotVersion = await(nextVersion[SnapshotRole](repoId))
    val delegations = await(signedRoleDelegationsFind.findSignedTargetRoleDelegations(targetRole))
    val snapshotRole = genSnapshotRole(signedRoot, targetRole, delegations, expireAt, snapshotVersion)
    val signedSnapshot = await(signRole(repoId, snapshotRole))

    val timestampVersion = await(nextVersion[TimestampRole](repoId))
    val timestampRole = genTimestampRole(signedSnapshot, expireAt, timestampVersion)
    val signedTimestamp = await(signRole(repoId, timestampRole))

    (signedSnapshot, signedTimestamp)
  }

  def addTargetItem(targetItem: TargetItem): Future[JsonSignedPayload] =
    targetRoleGeneration.addTargetItem(targetItem).flatMap(_ ⇒ regenerateAllSignedRoles(targetItem.repoId))

  def deleteTargetItem(repoId: RepoId, filename: TargetFilename): Future[Unit] = for {
    _ <- ensureTargetsCanBeSigned(repoId)
    _ <- targetRoleGeneration.deleteTargetItem(repoId, filename)
    _ <- regenerateAllSignedRoles(repoId)
  } yield ()

  private def signRole[T : Encoder : TufRole](repoId: RepoId, role: T): Future[SignedRole[T]] =
    new RepoRoleSigner(repoId, keyserverClient).apply(role)

  private def ensureTargetsCanBeSigned(repoId: RepoId): Future[SignedRole[TargetsRole]] = async {
    val targetsRole = await(signedRoleRepository.find[TargetsRole](repoId)).role
    await(signRole(repoId, targetsRole))
  }

  private def fetchRootRole(repoId: RepoId): Future[SignedRole[RootRole]] =
    keyserverClient.fetchRootRole(repoId).map { rootRole =>
      SignedRole.withChecksum[RootRole](repoId, rootRole.asJsonSignedPayload, rootRole.signed.version, rootRole.signed.expires)
    }

  private def findFreshRole[T](repoId: RepoId)(refreshFn: => Future[SignedRole[T]])(implicit tufRole: TufRole[T]): Future[SignedRole[T]] = {
    signedRoleRepository.find[T](repoId).flatMap { role =>
      if (role.expiresAt.isBefore(Instant.now.plus(1, ChronoUnit.HOURS))) {
        refreshFn.recoverWith {
          case KeyserverClient.RoleKeyNotFound =>
            log.info(s"Could not update ${tufRole.roleType} (for $repoId) because the keys are missing, returning expired version")
            FastFuture.successful(role)
        }
      } else
        FastFuture.successful(role)

    }.recoverWith {
      case DbSignedRoleRepository.SignedRoleNotFound =>
        generateAndCacheRole[T](repoId)
    }
  }

  private def generateAndCacheRole[T : TufRole](repoId: RepoId): Future[SignedRole[T]] = {
    regenerateAllSignedRoles(repoId)
      .recoverWith { case err => log.warn("Could not generate signed roles", err) ; FastFuture.failed(SignedRoleNotFound) }
      .flatMap(_ => signedRoleRepository.find[T](repoId))
  }

  protected [http] def findRole[T](repoId: RepoId)(implicit tufRole: TufRole[T]): Future[SignedRole[T]] = {
    findRole(repoId, tufRole.roleType).asInstanceOf[Future[SignedRole[T]]]
  }

  def findRole(repoId: RepoId, roleType: RoleType): Future[SignedRole[_]] = {
    lazy val refresher = new RepoRoleRefresh(keyserverClient)

    roleType match {
      case RoleType.ROOT =>
        fetchRootRole(repoId)
      case RoleType.TARGETS =>
        findFreshRole[TargetsRole](repoId)(refresher.refreshTargets(repoId))
      case RoleType.SNAPSHOT =>
        findFreshRole[SnapshotRole](repoId)(refresher.refreshSnapshots(repoId))
      case RoleType.TIMESTAMP =>
        findFreshRole[TimestampRole](repoId)(refresher.refreshTimestamp(repoId))
      case r =>
        throw new IllegalArgumentException(s"Unknown role type $r")
    }
  }

  private def nextVersion[T : TufRole](repoId: RepoId): Future[Int] =
    signedRoleRepository
      .find[T](repoId)
      .map(_.version + 1)
      .recover {
        case SignedRoleNotFound => 1
      }

  private def genSnapshotRole(root: SignedRole[RootRole], target: SignedRole[TargetsRole],
                              delegations: Map[MetaPath, MetaItem], expireAt: Instant, version: Int): SnapshotRole = {
    val meta = List(root.asMetaRole, target.asMetaRole).toMap ++ delegations
    SnapshotRole(meta, expireAt, version)
  }

  private def genTimestampRole(snapshotRole: SignedRole[SnapshotRole], expireAt: Instant, version: Int): TimestampRole = {
    val meta = Map(snapshotRole.asMetaRole)
    TimestampRole(meta, expireAt, version)
  }

  private def defaultExpire: Instant =
    Instant.now().plus(31, ChronoUnit.DAYS)

  private def findTargetRoleDelegations(repoId: RepoId): Future[Option[Delegations]] =
    signedRoleRepository.find[TargetsRole](repoId).map { signedTargetRole =>
      signedTargetRole.role.delegations
    }.recover {
      case SignedRoleNotFound => None
    }
}

protected class TargetRoleGeneration(roleSigningClient: KeyserverClient)
                          (implicit val db: Database, val ec: ExecutionContext)
  extends TargetItemRepositorySupport with FilenameCommentRepository.Support {

  def addTargetItem(targetItem: TargetItem): Future[TargetItem] =
    targetItemRepo.persist(targetItem)

  def deleteTargetItem(repoId: RepoId, filename: TargetFilename): Future[Unit] =
    targetItemRepo.deleteItemAndComments(filenameCommentRepo)(repoId, filename)

  def generate(repoId: RepoId, delegations: Option[Delegations], expireAt: Instant, version: Int): Future[TargetsRole] = {
    targetItemRepo.findFor(repoId).map { targetItems =>
      val targets = targetItems.map { item =>
        val hashes = Map(item.checksum.method -> item.checksum.hash)
        item.filename -> ClientTargetItem(hashes, item.length, item.custom.map(_.asJson))
      }.toMap

      TargetsRole(expireAt, targets, version, delegations)
    }
  }
}

