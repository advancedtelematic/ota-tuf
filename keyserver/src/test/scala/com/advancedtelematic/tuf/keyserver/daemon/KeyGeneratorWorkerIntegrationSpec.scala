package com.advancedtelematic.tuf.keyserver.daemon

import akka.actor.{ActorSystem, Status}
import akka.stream.ActorMaterializer
import akka.testkit.{ImplicitSender, TestKitBase}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, KeyGenId, KeyGenRequest}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport}
import org.scalatest.time.{Millis, Seconds, Span}

import scala.concurrent.ExecutionContext

class KeyGeneratorWorkerIntegrationSpec extends TufKeyserverSpec
  with TestKitBase
  with DatabaseSpec
  with ImplicitSender
  with KeyRepositorySupport
  with KeyGenRequestSupport {

  override implicit lazy val system: ActorSystem = ActorSystem("KeyGeneratorWorkerIntegrationSpec")

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig(timeout = Span(20, Seconds), interval = Span(300, Millis))

  implicit val mat = ActorMaterializer()

  lazy val vault = VaultClient(vaultAddr, vaultToken, vaultMount)

  val actorRef = system.actorOf(KeyGeneratorWorker.props(vault))

  test("adds key to vault") {
    val keyid = KeyGenId.generate()
    val repoId = RepoId.generate()
    val request = KeyGenRequest(keyid, repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT, keySize = 2048)
    keyGenRepo.persist(request)
    actorRef ! request

    val key = expectMsgPF() {
      case Status.Success(t: Seq[Key] @unchecked) => t.head
    }

    vault.findKey(key.id).futureValue.publicKey should include("BEGIN PUBLIC KEY")
    vault.findKey(key.id).futureValue.privateKey should include("BEGIN RSA PRIVATE KEY")
  }
}
