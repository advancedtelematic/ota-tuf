package com.advancedtelematic.tuf.keyserver.daemon

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKitBase}
import com.advancedtelematic.libtuf.data.TufDataType.{KeyType, RepoId, RoleType, RsaKeyType}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, KeyGenId, KeyGenRequest, KeyGenRequestStatus}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.tuf.util.{KeyTypeSpecSupport, TufKeyserverSpec}
import com.advancedtelematic.libats.test.DatabaseSpec
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
  with KeyTypeSpecSupport
  with Inspectors {

  override implicit lazy val system: ActorSystem = ActorSystem(this.getClass.getSimpleName)

  implicit val ec = ExecutionContext.global

  val testKeyGenOp: KeyGenRequest => Future[Seq[Key]] = (kgr: KeyGenRequest) => {
    val defaultOp = DefaultKeyGenerationOp()

    if(kgr.keySize > 2048)
      Future.failed(new Exception("test: Key size too big"))
    else
      defaultOp(kgr)
  }

  lazy val actorRef = system.actorOf(KeyGeneratorLeader.props(testKeyGenOp))

  override implicit def patienceConfig = PatienceConfig(timeout = Span(30, Seconds), interval = Span(300, Millis))

  override def beforeAll(): Unit = {
    super.beforeAll()
    actorRef
  }

  override def afterAll(): Unit = {
    super.afterAll()
    system.terminate()
  }

  private val timeout = Timeout(Span(30, Seconds))

  private val interval = Interval(Span(300, Milliseconds))

  def expectGenerated(newStatus: KeyGenRequestStatus, kt: KeyType, size: Int): Assertion = {
    val repoId = RepoId.generate()
    val keyGenRequest = KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT,
      keyType = kt, keySize = size)

    keyGenRepo.persist(keyGenRequest).futureValue
    eventually(timeout, interval) {
      keyGenRepo.find(keyGenRequest.id).futureValue.status shouldBe newStatus
    }
  }

  test("recovers when single worker fails") {
    // this only gets to the generator for RSA
    expectGenerated(KeyGenRequestStatus.ERROR, RsaKeyType, size = Int.MaxValue)

    expectGenerated(KeyGenRequestStatus.GENERATED, RsaKeyType, 2048)

    expectGenerated(KeyGenRequestStatus.GENERATED, RsaKeyType, 2048)
  }

  keyTypeTest("processes pending generation requests ") { keyType =>
      expectGenerated(KeyGenRequestStatus.GENERATED, keyType, keyType.crypto.defaultKeySize)
    }

  keyTypeTest("retries periodically for new pending requests ") { keyType =>
    expectGenerated(KeyGenRequestStatus.GENERATED, keyType, keyType.crypto.defaultKeySize)

    val keyGenReqs = Future.sequence {
      (1 to 20).map { _ =>
        val repoId = RepoId.generate()
        val otherKeyGenReq = KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT,
          keyType = keyType, keySize = keyType.crypto.defaultKeySize)
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
}
