package com.advancedtelematic.tuf.keyserver.daemon

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKitBase}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{KeyGenId, KeyGenRequest}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import com.advancedtelematic.libats.test.{DatabaseSpec, LongTest}
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport}
import org.scalatest.{Assertion, BeforeAndAfterAll, Inspectors}
import org.scalatest.concurrent.PatienceConfiguration.{Interval, Timeout}
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Millis, Milliseconds, Seconds, Span}

import scala.concurrent.{ExecutionContext, Future}

class KeyGeneratorLeaderSpec extends TufKeyserverSpec with TestKitBase with DatabaseSpec with ImplicitSender
  with KeyRepositorySupport
  with KeyGenRequestSupport
  with Eventually
  with BeforeAndAfterAll
  with Inspectors {

  override implicit lazy val system: ActorSystem = ActorSystem(this.getClass.getSimpleName)

  implicit val ec = ExecutionContext.global

  lazy val actorRef = system.actorOf(KeyGeneratorLeader.props(fakeVault))

  override implicit def patienceConfig = PatienceConfig(timeout = Span(20, Seconds), interval = Span(100, Millis))

  override def beforeAll(): Unit = {
    super.beforeAll()
    actorRef
  }

  override def afterAll(): Unit = {
    super.afterAll()
    system.terminate()
  }

  private val timeout = Timeout(Span(10, Seconds))

  private val interval = Interval(Span(200, Milliseconds))

  test("processes pending generation requests") {
    expectGenerated(KeyGenRequestStatus.GENERATED)
  }

  test("retries periodically for new pending requests") {
    expectGenerated(KeyGenRequestStatus.GENERATED)

    val keyGenReqs = Future.sequence {
      (1 to 20).map { _ =>
        val repoId = RepoId.generate()
        val otherKeyGenReq = KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT, keySize = 2048)
        keyGenRepo.persist(otherKeyGenReq).map(_.id)
      }
    }

    eventually(timeout, interval) {
      val ids = keyGenReqs.flatMap { ids => Future.sequence(ids.map(keyGenRepo.find)) }.futureValue

      forAll(ids) { id =>
        id.status shouldBe KeyGenRequestStatus.GENERATED
      }
    }
  }

  test("recovers when single worker fails") {
    expectGenerated(KeyGenRequestStatus.ERROR, size = -1)

    expectGenerated(KeyGenRequestStatus.GENERATED)

    expectGenerated(KeyGenRequestStatus.GENERATED)
  }

  def expectGenerated(newStatus: KeyGenRequestStatus, size: Int = 2048): Assertion = {
    val repoId = RepoId.generate()
    val keyGenRequest = KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT, keySize = size)
    keyGenRepo.persist(keyGenRequest).futureValue
    eventually(timeout, interval) {
      keyGenRepo.find(keyGenRequest.id).futureValue.status shouldBe newStatus
    }
  }
}
