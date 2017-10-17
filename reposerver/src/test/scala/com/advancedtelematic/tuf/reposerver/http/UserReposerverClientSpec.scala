package com.advancedtelematic.tuf.reposerver.http

import java.net.URI
import java.time.Instant

import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, RepoId, RoleType, SignedPayload, TufPrivateKey}
import com.advancedtelematic.tuf.reposerver.db.RepoNamespaceRepositorySupport
import com.advancedtelematic.tuf.reposerver.util.{ResourceSpec, TufReposerverSpec}
import org.scalatest.time.{Seconds, Span}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.reposerver.UserReposerverHttpClient

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class UserReposerverClientSpec extends TufReposerverSpec
  with ResourceSpec
  with FakeScalajHttpClient
  with RepoNamespaceRepositorySupport {

  override def executor: ExecutionContextExecutor = super.executor

  implicit val ec: ExecutionContext = system.dispatcher

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(30, Seconds))

  val repoId = RepoId.generate()

  val client = new UserReposerverHttpClient(URI.create("http://localhost"), testClient, "")

  override def beforeAll(): Unit = {
    super.beforeAll()
    fakeKeyserverClient.createRoot(repoId)
      .flatMap(_ => repoNamespaceRepo.persist(repoId, Namespace("default")))
      .futureValue
  }

  test("fetches a root") {
    val signedRoot = client.root().futureValue
    signedRoot shouldBe a[SignedPayload[_]]
    signedRoot.signed shouldBe a[RootRole]
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
    val f = for {
      signedRoot <- client.root()
      oldKey <- client.deleteKey(signedRoot.signed.roles(RoleType.ROOT).keyids.head)
    } yield oldKey

    f.futureValue shouldBe a[TufPrivateKey]
  }

  test("pushes a target key") {
    val (newKey, _) = TufCrypto.generateKeyPair(EdKeyType, 256)

    val f = for {
      _ <- client.pushTargetsKey(newKey)
      newRoot <- fakeKeyserverClient.fetchUnsignedRoot(repoId)
    } yield newRoot

    val newRoot = f.futureValue

    newRoot.roles(RoleType.TARGETS).keyids should contain(newKey.id)
  }
}
