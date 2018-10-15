package com.advancedtelematic.tuf.reposerver.http

import java.time.Instant

import akka.http.scaladsl.util.FastFuture
import cats.syntax.either._
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{TimestampRole, _}
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, RepoId, RoleType}
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepositorySupport
import com.advancedtelematic.tuf.reposerver.util.{FakeKeyserverClient, TufReposerverSpec}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits


class SignedRoleGenerationSpec extends TufReposerverSpec with DatabaseSpec with SignedRoleRepositorySupport with ScalaFutures {
  override implicit val ec: ExecutionContext = Implicits.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  val fakeKeyserverClient: FakeKeyserverClient = new FakeKeyserverClient

  val signedRoleGeneration = new SignedRoleGeneration(fakeKeyserverClient)

  def setupRepo(): Future[RepoId] = for {
    repoId <- FastFuture.successful(RepoId.generate())
    _ <- fakeKeyserverClient.createRoot(repoId, Ed25519KeyType)
    _ <- signedRoleGeneration.regenerateAllSignedRoles(repoId)
  } yield repoId

  test("renewal of snapshots renews also timestamps") {
    val repoId = setupRepo().futureValue

    val oldTimestamps = signedRoleGeneration.findRole(repoId, RoleType.TIMESTAMP).futureValue
    val oldSnapshots = signedRoleGeneration.findRole(repoId, RoleType.SNAPSHOT).futureValue

    signedRoleRepo.persist(oldSnapshots.copy(expireAt = Instant.now().minusSeconds(60)), forceVersion = true).futureValue

    val renewedSnapshots = signedRoleGeneration.findRole(repoId, RoleType.SNAPSHOT).futureValue
    val renewedTimestampsDb = signedRoleRepo.find(repoId, RoleType.TIMESTAMP).futureValue
    val (_, renewedSnapshotsHash) = renewedTimestampsDb.content.signed.as[TimestampRole].valueOr(throw _).meta(RoleType.SNAPSHOT.toMetaPath).hashes.head

    renewedSnapshots.checksum shouldNot be(oldSnapshots.checksum)
    renewedSnapshots.version shouldBe oldSnapshots.version + 1

    renewedTimestampsDb.version shouldBe oldTimestamps.version + 1
    renewedSnapshotsHash shouldBe renewedSnapshots.checksum.hash
  }

  test("renewal of targets renews also snapshots and timestamps") {
    val repoId = setupRepo().futureValue

    val oldTargets = signedRoleGeneration.findRole(repoId, RoleType.TARGETS).futureValue
    val oldSnapshots = signedRoleGeneration.findRole(repoId, RoleType.SNAPSHOT).futureValue
    val oldTimestamps = signedRoleGeneration.findRole(repoId, RoleType.TIMESTAMP).futureValue

    signedRoleRepo.persist(oldTargets.copy(expireAt = Instant.now().minusSeconds(60)), forceVersion = true).futureValue

    val renewedTargets = signedRoleGeneration.findRole(repoId, RoleType.TARGETS).futureValue
    val renewedSnapshots = signedRoleRepo.find(repoId, RoleType.SNAPSHOT).futureValue
    val renewedTimestamps = signedRoleGeneration.findRole(repoId, RoleType.TIMESTAMP).futureValue
    val (_, renewedSnapshotsHash) = renewedTimestamps.content.signed.as[TimestampRole].valueOr(throw _).meta(RoleType.SNAPSHOT.toMetaPath).hashes.head

    // TODO:SM This is the part that should be somehow abstracted and clean because a SignedRole should always be parseable to a Role
    val renewedSnaphotsParsed = renewedSnapshots.content.signed.as[SnapshotRole].valueOr(throw _)
    val (_, renewedTargetsHash) = renewedSnaphotsParsed.meta(RoleType.TARGETS.toMetaPath).hashes.head

    renewedTargetsHash shouldBe renewedTargets.checksum.hash
    renewedSnapshots.version shouldBe oldSnapshots.version + 1

    renewedTimestamps.version shouldBe oldTimestamps.version + 1
    renewedSnapshotsHash shouldBe renewedSnapshots.checksum.hash
  }
}
