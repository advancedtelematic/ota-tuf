package com.advancedtelematic.tuf.reposerver.http

import java.net.URI
import java.time.Instant

import cats.syntax.either._
import eu.timepit.refined._
import com.advancedtelematic.libats.data.DataType.{Namespace, ValidChecksum}
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, KeyType, RepoId, RoleType, RsaKeyType, SignedPayload, TufKey, TufPrivateKey}
import com.advancedtelematic.tuf.reposerver.db.RepoNamespaceRepositorySupport
import com.advancedtelematic.tuf.reposerver.util._
import org.scalatest.time.{Seconds, Span}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.http.SHttpjServiceClient.HttpjClientError
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient.RoleChecksumNotValid
import com.advancedtelematic.libtuf.reposerver.UserReposerverHttpClient
import org.scalatest.BeforeAndAfter

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class UserReposerverClientSpec(keyType: KeyType) extends TufReposerverSpec
  with ResourceSpec
  with FakeScalajHttpClient
  with RepoNamespaceRepositorySupport
  with BeforeAndAfter {

  override def executor: ExecutionContextExecutor = super.executor

  implicit val ec: ExecutionContext = system.dispatcher

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(60, Seconds))

  val repoId = RepoId.generate()

  val client = new UserReposerverHttpClient(URI.create("http://test-reposerver"), testClient, token = None)


  override def beforeAll(): Unit = {
    super.beforeAll()
    repoNamespaceRepo.persist(repoId, Namespace("default")).futureValue
  }

  before {
    fakeKeyserverClient.createRoot(repoId, keyType).futureValue
  }

  after {
    fakeKeyserverClient.resetKeyServer()
  }

  test("fetches a root") {
    val signedRoot = client.root().futureValue
    signedRoot shouldBe a[SignedPayload[_]]
    signedRoot.signed shouldBe a[RootRole]
  }

  test("fetches a root by version") {
    val oldRoot = client.root().futureValue
    val newRoot = client.root(Some(oldRoot.signed.version)).futureValue
    newRoot.signed.version shouldBe oldRoot.signed.version
  }

  test("accepts old root") {
    val signedRoot = client.root().futureValue
    client.pushSignedRoot(signedRoot).futureValue
  }

  test("accepts a new targets role") {
    val targets = TargetsRole(Instant.now, Map.empty, 20)
    val signedTargets = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, targets).futureValue
    client.pushTargets(signedTargets, None).futureValue
  }

  test("moves key offline") {
    val signedRoot = client.root().futureValue
    val keyPair = client.fetchKeyPair(signedRoot.signed.roles(RoleType.ROOT).keyids.head).futureValue
    client.deleteKey(signedRoot.signed.roles(RoleType.ROOT).keyids.head).futureValue

    keyPair.privkey shouldBe a[TufPrivateKey]
    keyPair.pubkey shouldBe a[TufKey]

    client.fetchKeyPair(keyPair.pubkey.id).failed.futureValue shouldBe a[HttpjClientError]
  }

  test("returns specific exception when previous checksum is not valid") {
    val targets = TargetsRole(Instant.now, Map.empty, 20)
    val signedTargets = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, targets).futureValue
    val invalidChecksum = refineV[ValidChecksum]("11c3599621d7edc417c795363767754b431404e8f9fd6fb85f78b2b45423b00b").valueOr(err => throw new Exception(err))
    client.pushTargets(signedTargets, Option(invalidChecksum)).failed.futureValue shouldBe RoleChecksumNotValid
  }

  test("returns specific exception when no previous checksum is present at all") {
    val targets = TargetsRole(Instant.now, Map.empty, 20)
    val signedTargets = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, targets).futureValue
    client.pushTargets(signedTargets, None).failed.futureValue shouldBe RoleChecksumNotValid
  }

  test("can update with proper checksum header") {
    val targetsResponse = client.targets().futureValue

    val targets = TargetsRole(Instant.now, Map.empty, targetsResponse.targets.signed.version + 1)
    val signedTargets = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, targets).futureValue

    client.pushTargets(signedTargets, targetsResponse.checksum).futureValue shouldBe (())
  }

  test("can pull targets") {
    val targetsResponse = client.targets().futureValue
    targetsResponse.targets.signatures shouldNot be(empty)
    targetsResponse.checksum shouldNot be(empty)
  }
}

class RsaUserReposerverClientSpec extends UserReposerverClientSpec(RsaKeyType)

class EdUserReposerverClientSpec extends UserReposerverClientSpec(Ed25519KeyType)
