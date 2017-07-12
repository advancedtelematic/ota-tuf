package com.advancedtelematic.tuf.keyserver.daemon

import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, EdTufKey, KeyType, RepoId, RoleType, RsaKeyType}
import akka.actor.{ActorSystem, Status}
import akka.testkit.{ImplicitSender, TestKitBase}
import com.advancedtelematic.libtuf.crypt.TufCrypto._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import com.advancedtelematic.libats.http.Errors.MissingEntity
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport}
import com.advancedtelematic.libtuf.data.TufCodecs._
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Milliseconds, Seconds, Span}
import io.circe.syntax._
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._


class KeyGeneratorWorkerSpec extends TufKeyserverSpec with TestKitBase with DatabaseSpec with ImplicitSender
  with KeyRepositorySupport
  with KeyGenRequestSupport
  with PatienceConfiguration {
  override implicit lazy val system: ActorSystem = ActorSystem("KeyGeneratorWorkerIntegrationSpec")

  implicit val ec = ExecutionContext.global

  val actorRef = system.actorOf(KeyGeneratorWorker.props(fakeVault))

  private val timeout = Span(20, Seconds)

  override implicit def patienceConfig =
    PatienceConfig().copy(timeout = timeout, interval = Span(300, Milliseconds))

  def keyGenRequest(threshold: Int = 1, keyType: KeyType = RsaKeyType): Future[KeyGenRequest] = {
    val keyGenId = KeyGenId.generate()
    val repoId = RepoId.generate()
    val request = KeyGenRequest(keyGenId, repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT,
                                keySize = 2048, keyType = keyType, threshold = threshold)
    keyGenRepo.persist(request)
  }

  test("generates a key for a key gen request") {
    val keyGenReq = keyGenRequest().futureValue
    actorRef ! keyGenReq

    val key = expectMsgPF(timeout) {
      case Status.Success(t: Seq[Key] @unchecked) => t.head
    }

    keyGenRepo.find(keyGenReq.id).futureValue.status shouldBe KeyGenRequestStatus.GENERATED

    keyRepo.find(key.id).futureValue.publicKey.toPem should include("BEGIN PUBLIC KEY")
  }

  test("generates a ed25519 key") {
    val keyGenReq = keyGenRequest(keyType = EdKeyType).futureValue
    actorRef ! keyGenReq

    val key = expectMsgPF(timeout) {
      case Status.Success(t: Seq[Key] @unchecked) => t.head
      case Status.Failure(ex) ⇒ throw ex
    }

    keyGenRepo.find(keyGenReq.id).futureValue.status shouldBe KeyGenRequestStatus.GENERATED

    val dbKey = keyRepo.find(key.id).futureValue

    dbKey.keyType shouldBe EdKeyType
    dbKey.publicKey.toPem should include("BEGIN PUBLIC KEY")

    TufCrypto.edCrypto.parsePublic(Hex.toHexString(dbKey.publicKey.getEncoded)).get shouldBe a[EdTufKey]
  }

  test("associates new key with role") {
    val keyGenReq = keyGenRequest().futureValue
    actorRef ! keyGenReq

    val key = expectMsgPF(timeout) {
      case Status.Success(t: Seq[Key] @unchecked) => t.head
    }

    val keys = keyRepo.repoKeys(keyGenReq.repoId).futureValue

    keys.map(_.id) should contain(key.id)
  }

  test("sends back Failure if something bad happens") {
    val repoId = RepoId.generate()
    actorRef ! KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT, keyType = RsaKeyType, keySize = 2048)
    val exception = expectMsgType[Status.Failure](3.seconds)
    exception.cause shouldBe a[MissingEntity[_]]
  }

  test("keys with an error are marked as error") {
    val keyGenId = KeyGenId.generate()
    val repoId = RepoId.generate()
    val kgr = keyGenRepo.persist(KeyGenRequest(keyGenId, repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT, keyType = RsaKeyType, keySize = -1)).futureValue
    actorRef ! kgr

    val exception = expectMsgType[Status.Failure](3.seconds)
    exception.cause shouldBe a[IllegalArgumentException]

    keyGenRepo.find(keyGenId).futureValue.status shouldBe KeyGenRequestStatus.ERROR
  }

  test("adds key to vault") {
    actorRef ! keyGenRequest().futureValue

    val key = expectMsgPF(timeout) {
      case Status.Success(t: Seq[Key] @unchecked) => t.head
      case Status.Failure(ex) => fail(ex)
    }

    fakeVault.findKey(key.id).futureValue.publicKey should include("BEGIN PUBLIC KEY")
    fakeVault.findKey(key.id).futureValue.privateKey.asJson.noSpaces should include("BEGIN RSA PRIVATE KEY")
  }

  test("threshold keys per role") {
    actorRef ! keyGenRequest(5).futureValue

    val keys = expectMsgPF(timeout) {
      case Status.Success(t: Seq[Key] @unchecked) => t
      case Status.Failure(ex) => fail(ex)
    }

    keys.map(_.id).distinct.size shouldBe 5
  }
}
