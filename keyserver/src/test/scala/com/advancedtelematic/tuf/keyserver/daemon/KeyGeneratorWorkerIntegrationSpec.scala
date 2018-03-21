package com.advancedtelematic.tuf.keyserver.daemon

import akka.actor.{ActorSystem, Status}
import akka.stream.ActorMaterializer
import akka.testkit.{ImplicitSender, TestKitBase}
import com.advancedtelematic.libtuf.crypt.TufCrypto._
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, KeyType, RepoId, RoleType, RsaKeyType}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, KeyGenId, KeyGenRequest}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport}
import org.scalatest.time.{Millis, Seconds, Span}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient

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

  val actorRef = system.actorOf(KeyGeneratorWorker.props(DefaultKeyGenerationOp(vault)))

  def keySpecific(keyType: KeyType, name: String, privTag: String) {
    test("adds key to vault " + name) {
      val keyid = KeyGenId.generate()
      val repoId = RepoId.generate()
      val request = KeyGenRequest(keyid, repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT, keyType = keyType,
                                  keySize = keyType.crypto.defaultKeySize)
      keyGenRepo.persist(request)
      actorRef ! request

      val key = expectMsgPF() {
        case Status.Success(t: Seq[Key]@unchecked) => t.head
      }

      val vaultKey = vault.findKey(key.id).futureValue
      val pub = vaultKey.publicKey.keyval.toPem
      pub should include("BEGIN PUBLIC KEY")
      val priv = vaultKey.privateKey.keyval.toPem
      priv should include(s"BEGIN $privTag PRIVATE KEY")
    }
  }

  testsFor(keySpecific(RsaKeyType, "RSA", "RSA"))
  testsFor(keySpecific(Ed25519KeyType, "Ed25519", "EC"))
}
