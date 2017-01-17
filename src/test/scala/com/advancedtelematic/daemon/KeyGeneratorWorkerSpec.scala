package com.advancedtelematic.daemon

import akka.actor.{ActorSystem, Status}
import akka.testkit.{ImplicitSender, TestKitBase}
import com.advancedtelematic.ota_tuf.daemon.KeyGeneratorWorker
import com.advancedtelematic.ota_tuf.data.DataType.{KeyGenRequest, KeyId}
import com.advancedtelematic.ota_tuf.data.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.db.{KeyGenRequestSupport, KeyRepositorySupport}
import com.advancedtelematic.util.OtaTufSpec
import org.genivi.sota.core.DatabaseSpec

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class KeyGeneratorWorkerSpec extends OtaTufSpec with TestKitBase with DatabaseSpec with ImplicitSender
  with KeyRepositorySupport
  with KeyGenRequestSupport {
  override implicit lazy val system: ActorSystem = ActorSystem("KeyGeneratorWorkerIntegrationSpec")

  implicit val ec = ExecutionContext.global

  val actorRef = system.actorOf(KeyGeneratorWorker.props(fakeVault))

  test("generates a key for a key gen request") {
    val keyid = KeyId.generate()
    actorRef ! KeyGenRequest(keyid, KeyGenRequestStatus.REQUESTED)
    expectMsgType[Status.Success](3.seconds)
    keyGenRepo.find(keyid).futureValue.status shouldBe KeyGenRequestStatus.GENERATED
    keyRepo.find(keyid).futureValue.publicKey should include("BEGIN PUBLIC KEY")
  }

  test("sends back Failure if something bad happens") {
    actorRef ! KeyGenRequest(KeyId.generate(), KeyGenRequestStatus.REQUESTED, size = -1)
    val exception = expectMsgType[Status.Failure](3.seconds)
    exception.cause shouldBe a[IllegalArgumentException]
  }

  test("keys with an error are marked as error") {
    val keyId = KeyId.generate()
    val kgr = keyGenRepo.persist(KeyGenRequest(keyId, KeyGenRequestStatus.REQUESTED, size = -1)).futureValue
    actorRef ! kgr

    val exception = expectMsgType[Status.Failure](3.seconds)
    exception.cause shouldBe a[IllegalArgumentException]

    keyGenRepo.find(keyId).futureValue.status shouldBe KeyGenRequestStatus.ERROR
  }

  test("adds key to vault") {
    val keyid = KeyId.generate()
    actorRef ! KeyGenRequest(keyid, KeyGenRequestStatus.REQUESTED)
    expectMsgType[Status.Success](3.seconds)

    fakeVault.findKey(keyid).futureValue.publicKey should include("BEGIN PUBLIC KEY")
    fakeVault.findKey(keyid).futureValue.privateKey should include("BEGIN RSA PRIVATE KEY")
  }
}
