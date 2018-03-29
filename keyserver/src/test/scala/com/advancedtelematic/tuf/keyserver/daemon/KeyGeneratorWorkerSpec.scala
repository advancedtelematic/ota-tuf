package com.advancedtelematic.tuf.keyserver.daemon

import com.advancedtelematic.libtuf.data.TufDataType.KeyId
import akka.actor.{ActorSystem, Props, Status}
import akka.testkit.{ImplicitSender, TestKitBase, TestProbe}
import com.advancedtelematic.libtuf.crypt.TufCrypto._
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, Ed25519TufKey, KeyType, RepoId, RoleType}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.util.{KeyTypeSpecSupport, TufKeyserverSpec}
import com.advancedtelematic.libats.http.Errors.MissingEntity
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport}
import com.advancedtelematic.libtuf.data.TufCodecs._
import io.circe.syntax._
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Milliseconds, Seconds, Span}
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._


class KeyGeneratorWorkerSpec extends TufKeyserverSpec with TestKitBase with DatabaseSpec with ImplicitSender
  with KeyRepositorySupport
  with KeyGenRequestSupport
  with KeyTypeSpecSupport
  with PatienceConfiguration {
  override implicit lazy val system: ActorSystem = ActorSystem("KeyGeneratorWorkerIntegrationSpec")

  implicit val ec = ExecutionContext.global

  val actorRef = system.actorOf(KeyGeneratorWorker.props(DefaultKeyGenerationOp(fakeVault)))

  private val timeout = Span(20, Seconds)

  def findKey(keyId: KeyId): Future[Key] =
    keyRepo.findAll(Seq(keyId)).map(_.head)

  override implicit def patienceConfig =
    PatienceConfig().copy(timeout = timeout, interval = Span(300, Milliseconds))

  def keyGenRequest(threshold: Int = 1)(implicit keyType: KeyType): Future[KeyGenRequest] = {
    val keyGenId = KeyGenId.generate()
    val repoId = RepoId.generate()
    val request = KeyGenRequest(keyGenId, repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT,
      keySize = keyType.crypto.defaultKeySize, keyType = keyType, threshold = threshold)
    keyGenRepo.persist(request)
  }

  keyTypeTest("generates a key for a key gen request ") { implicit keyType =>
    val keyGenReq = keyGenRequest().futureValue
    actorRef ! keyGenReq

    val key = expectMsgPF(timeout) {
      case Status.Success(t: Seq[Key]@unchecked) => t.head
      case Status.Failure(ex) ⇒ throw ex
    }

    keyGenRepo.find(keyGenReq.id).futureValue.status shouldBe KeyGenRequestStatus.GENERATED

    val dbKey = keyRepo.findAll(Seq(key.id)).futureValue.head

    dbKey.keyType shouldBe keyType
    dbKey.publicKey.toPem should include("BEGIN PUBLIC KEY")

    if (keyType == Ed25519KeyType) {
      keyType.crypto.parsePublic(Hex.toHexString(dbKey.publicKey.getEncoded)).get shouldBe a[Ed25519TufKey]
    }
  }

  keyTypeTest("associates new key with role") { implicit keyType =>
    val keyGenReq = keyGenRequest().futureValue
    actorRef ! keyGenReq

    val key = expectMsgPF(timeout) {
      case Status.Success(t: Seq[Key]@unchecked) => t.head
    }

    val keys = keyRepo.repoKeys(keyGenReq.repoId).futureValue

    keys.map(_.id) should contain(key.id)
  }

  keyTypeTest("sends back Failure if something bad happens ") { implicit keyType =>
    val repoId = RepoId.generate()
    actorRef ! KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT,
      keyType = keyType, keySize = keyType.crypto.defaultKeySize)
    val exception = expectMsgType[Status.Failure](3.seconds)
    exception.cause shouldBe a[MissingEntity[_]]
  }

  keyTypeTest("keys with an error are marked as error ") { implicit keyType =>
    val keyGenId = KeyGenId.generate()
    val repoId = RepoId.generate()

    val probe = TestProbe()

    val alwaysErrorActor = system.actorOf(Props(new KeyGeneratorWorker(
      _ => Future.failed(new Exception("test: key gen failed"))
    )))

    val kgr = keyGenRepo.persist(KeyGenRequest(keyGenId, repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT,
      keyType = keyType, keySize = keyType.crypto.defaultKeySize)).futureValue
    alwaysErrorActor.tell(kgr, probe.ref)

    val exception = probe.expectMsgType[Status.Failure](3.seconds)
    exception.cause.getMessage shouldBe "test: key gen failed"

    keyGenRepo.find(keyGenId).futureValue.status shouldBe KeyGenRequestStatus.ERROR
  }

  keyTypeTest("adds key to vault ") { implicit keyType =>
    actorRef ! keyGenRequest().futureValue

    val key = expectMsgPF(timeout) {
      case Status.Success(t: Seq[Key]@unchecked) => t.head
      case Status.Failure(ex) => fail(ex)
    }

    val pubKey = fakeVault.findKey(key.id).futureValue.publicKey
    val privKey = fakeVault.findKey(key.id).futureValue.privateKey

    pubKey.keyval.toPem should include("BEGIN PUBLIC KEY")
    privKey.keyval.toPem should include("BEGIN")
    privKey.keyval.toPem should include("PRIVATE KEY")
  }

  keyTypeTest("threshold keys per role ") { implicit keyType =>
    actorRef ! keyGenRequest(5).futureValue

    val keys = expectMsgPF(timeout) {
      case Status.Success(t: Seq[Key]@unchecked) => t
      case Status.Failure(ex) => fail(ex)
    }

    keys.map(_.id).distinct.size shouldBe 5
  }

}
