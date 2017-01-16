package com.advancedtelematic.daemon

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKitBase}
import com.advancedtelematic.ota_tuf.daemon.KeyGeneratorLeader
import com.advancedtelematic.ota_tuf.data.DataType.{KeyGenRequest, KeyId}
import com.advancedtelematic.ota_tuf.data.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.db.{KeyGenRequestSupport, KeyRepositorySupport}
import com.advancedtelematic.util.OtaTufSpec
import org.genivi.sota.core.DatabaseSpec
import org.scalatest.{BeforeAndAfterAll, Inspectors}
import org.scalatest.concurrent.PatienceConfiguration.{Interval, Timeout}
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Milliseconds, Seconds, Span}

import scala.concurrent.{ExecutionContext, Future}

class KeyGeneratorLeaderSpec extends OtaTufSpec with TestKitBase with DatabaseSpec with ImplicitSender
  with KeyRepositorySupport
  with KeyGenRequestSupport
  with Eventually
  with BeforeAndAfterAll
  with Inspectors {
  override implicit lazy val system: ActorSystem = ActorSystem(this.getClass.getSimpleName)

  implicit val ec = ExecutionContext.global

  lazy val actorRef = system.actorOf(KeyGeneratorLeader.props())

  override def beforeAll(): Unit = {
    super.beforeAll()
    actorRef
  }

  override def afterAll(): Unit = {
    super.afterAll()
    system.terminate()
  }

  private val timeout = Timeout(Span(5, Seconds))

  private val interval = Interval(Span(200, Milliseconds))

  test("processes pending generation requests") {
    val keyGenReq = KeyGenRequest(KeyId.generate(), KeyGenRequestStatus.REQUESTED)
    keyGenRepo.persist(keyGenReq).futureValue

    eventually(timeout, interval) {
      keyGenRepo.find(keyGenReq.id).futureValue.status shouldBe KeyGenRequestStatus.GENERATED
    }
  }

  test("retries periodically for new pending requests") {
    val keyGenReq = KeyGenRequest(KeyId.generate(), KeyGenRequestStatus.REQUESTED)
    keyGenRepo.persist(keyGenReq).futureValue

    eventually(timeout, interval) {
      keyGenRepo.find(keyGenReq.id).futureValue.status shouldBe KeyGenRequestStatus.GENERATED
    }

    val keyGenReqs = Future.sequence {
      (1 to 20).map { _ ⇒
        val otherKeyGenReq = KeyGenRequest(KeyId.generate(), KeyGenRequestStatus.REQUESTED)
        keyGenRepo.persist(otherKeyGenReq).map(_.id)
      }
    }

    eventually(timeout, interval) {
      val ids = keyGenReqs.flatMap { ids ⇒ Future.sequence(ids.map(keyGenRepo.find)) }.futureValue

      forAll(ids) { id ⇒
        id.status shouldBe KeyGenRequestStatus.GENERATED
      }
    }
  }

  test("recovers when single worker fails") {
    val keyGenReq = KeyGenRequest(KeyId.generate(), KeyGenRequestStatus.REQUESTED, size = -1)
    keyGenRepo.persist(keyGenReq).futureValue

    eventually(timeout, interval) {
      keyGenRepo.find(keyGenReq.id).futureValue.status shouldBe KeyGenRequestStatus.ERROR
    }

    val otherKeyGenReq = KeyGenRequest(KeyId.generate(), KeyGenRequestStatus.REQUESTED)
    keyGenRepo.persist(otherKeyGenReq).futureValue
    eventually(timeout, interval) {
      keyGenRepo.find(otherKeyGenReq.id).futureValue.status shouldBe KeyGenRequestStatus.GENERATED
    }
  }
}
