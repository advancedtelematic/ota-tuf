package com.advancedtelematic.ota_tuf.daemon

import akka.actor.{ActorSystem, Status}
import akka.stream.ActorMaterializer
import akka.testkit.{ImplicitSender, TestKitBase}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType
import com.advancedtelematic.ota_tuf.data.KeyServerDataType.{Key, KeyGenId, KeyGenRequest}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.ota_tuf.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.db.{KeyGenRequestSupport, KeyRepositorySupport}
import com.advancedtelematic.ota_tuf.vault.VaultClient
import com.advancedtelematic.util.OtaTufSpec
import org.genivi.sota.core.DatabaseSpec

import scala.concurrent.ExecutionContext

class KeyGeneratorWorkerIntegrationSpec extends OtaTufSpec
  with TestKitBase
  with DatabaseSpec
  with ImplicitSender
  with KeyRepositorySupport
  with KeyGenRequestSupport {

  override implicit lazy val system: ActorSystem = ActorSystem("KeyGeneratorWorkerIntegrationSpec")

  implicit val ec = ExecutionContext.global

  implicit val mat = ActorMaterializer()

  lazy val vault = VaultClient(vaultAddr, vaultToken, vaultMount)

  val actorRef = system.actorOf(KeyGeneratorWorker.props(vault))

  test("adds key to vault") {
    val keyid = KeyGenId.generate()
    val repoId = RepoId.generate()
    val request = KeyGenRequest(keyid, repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT)
    keyGenRepo.persist(request)
    actorRef ! request

    val key = expectMsgPF() {
      case Status.Success(t: Key) => t
    }

    vault.findKey(key.id).futureValue.publicKey should include("BEGIN PUBLIC KEY")
    vault.findKey(key.id).futureValue.privateKey should include("BEGIN RSA PRIVATE KEY")
  }
}
