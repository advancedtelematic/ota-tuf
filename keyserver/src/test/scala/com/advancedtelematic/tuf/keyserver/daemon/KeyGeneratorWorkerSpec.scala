package com.advancedtelematic.tuf.keyserver.daemon

import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import akka.actor.{ActorSystem, Status}
import akka.testkit.{ImplicitSender, TestKitBase}
import com.advancedtelematic.libtuf.crypt.RsaKeyPair._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libats.http.Errors.MissingEntity
import cats.syntax.show.toShowOps
import com.advancedtelematic.libtuf.data.TufDataType.RoleType
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport}
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class KeyGeneratorWorkerSpec extends TufKeyserverSpec with TestKitBase with DatabaseSpec with ImplicitSender
  with KeyRepositorySupport
  with KeyGenRequestSupport
  with PatienceConfiguration {
  override implicit lazy val system: ActorSystem = ActorSystem("KeyGeneratorWorkerIntegrationSpec")

  implicit val ec = ExecutionContext.global

  val actorRef = system.actorOf(KeyGeneratorWorker.props(fakeVault))

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  def keyGenRequest: Future[KeyGenRequest] = {
    val keyGenId = KeyGenId.generate()
    val repoId = RepoId.generate()
    keyGenRepo.persist(KeyGenRequest(keyGenId, repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT))
  }

  test("generates a key for a key gen request") {
    val keyGenReq = keyGenRequest.futureValue
    actorRef ! keyGenReq

    val key = expectMsgPF() {
      case Status.Success(t: Key) => t
    }

    keyGenRepo.find(keyGenReq.id).futureValue.status shouldBe KeyGenRequestStatus.GENERATED

    keyRepo.find(key.id).futureValue.publicKey.show should include("BEGIN PUBLIC KEY")
  }

  test("associates new key with role") {
    val keyGenReq = keyGenRequest.futureValue
    actorRef ! keyGenReq

    val key = expectMsgPF() {
      case Status.Success(t: Key) => t
    }

    val keys = keyRepo.keysFor(keyGenReq.repoId).futureValue

    keys.map(_.id) should contain(key.id)
  }

  test("sends back Failure if something bad happens") {
    val repoId = RepoId.generate()
    actorRef ! KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT)
    val exception = expectMsgType[Status.Failure](3.seconds)
    exception.cause shouldBe a[MissingEntity]
  }

  test("keys with an error are marked as error") {
    val keyGenId = KeyGenId.generate()
    val repoId = RepoId.generate()
    val kgr = keyGenRepo.persist(KeyGenRequest(keyGenId, repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT, keySize = -1)).futureValue
    actorRef ! kgr

    val exception = expectMsgType[Status.Failure](3.seconds)
    exception.cause shouldBe a[IllegalArgumentException]

    keyGenRepo.find(keyGenId).futureValue.status shouldBe KeyGenRequestStatus.ERROR
  }

  test("adds key to vault") {
    actorRef ! keyGenRequest.futureValue

    val key = expectMsgPF() {
      case Status.Success(t: Key) => t
      case Status.Failure(ex) => fail(ex)
    }

    fakeVault.findKey(key.id).futureValue.publicKey should include("BEGIN PUBLIC KEY")
    fakeVault.findKey(key.id).futureValue.privateKey should include("BEGIN RSA PRIVATE KEY")
  }

  test("threshold keys per role") (pending)
}
