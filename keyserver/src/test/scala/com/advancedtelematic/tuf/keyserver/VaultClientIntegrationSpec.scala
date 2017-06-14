package com.advancedtelematic.tuf.keyserver

import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.TufDataType.KeyType
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.{VaultKey, VaultKeyNotFound}
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import RsaKeyPair._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.syntax.show.toShowOps
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

class VaultClientIntegrationSpec extends TufKeyserverSpec
  with PatienceConfiguration {

  implicit val _system = ActorSystem(this.getClass.getSimpleName)
  implicit val _mat = ActorMaterializer()

  import _system.dispatcher

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))

  lazy val vault = VaultClient(vaultAddr, vaultToken, vaultMount)

  test("creates/finds a key") {
    val rsaKey = RsaKeyPair.generate()

    vault.createKey(VaultKey(rsaKey.id, KeyType.RSA,
      rsaKey.getPublic.show,
      rsaKey.getPrivate.show)).futureValue

    vault.findKey(rsaKey.id).map(_.privateKey).futureValue shouldBe rsaKey.getPrivate.show
  }

  test("deletes a key") {
    val rsaKey = RsaKeyPair.generate()

    vault.createKey(VaultKey(rsaKey.id, KeyType.RSA,
      rsaKey.getPublic.show,
      rsaKey.getPrivate.show)).futureValue

    vault.deleteKey(rsaKey.id).futureValue

    vault.findKey(rsaKey.id).failed.futureValue shouldBe VaultKeyNotFound
  }

  test("renews the token") {
    vault.renewToken().futureValue shouldBe(())
  }
}
