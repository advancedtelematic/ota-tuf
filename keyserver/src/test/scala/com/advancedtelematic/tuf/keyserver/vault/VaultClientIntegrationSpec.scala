package com.advancedtelematic.tuf.keyserver.vault

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Path.Slash
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, StatusCodes, Uri}
import akka.stream.ActorMaterializer
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.crypt.TufCrypto._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, KeyType, RsaKeyType}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.{VaultKey, VaultResourceNotFound}
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import io.circe.Json
import io.circe.syntax._
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

class VaultClientIntegrationSpec extends TufKeyserverSpec
  with PatienceConfiguration {

  implicit val _system = ActorSystem(this.getClass.getSimpleName)
  implicit val _mat = ActorMaterializer()

  import _system.dispatcher

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))

  lazy val vault = VaultClient(vaultAddr, vaultToken, vaultMount)

  test("renews the token") {
    vault.renewToken().futureValue shouldBe(())
  }

  test("can decode a legacy key") {
    val pair = TufCrypto.generateKeyPair(RsaKeyType, 2048)

    val vaultKey = VaultKey(pair.pubkey.id, Ed25519KeyType, pair.pubkey, pair.privkey)

    val legacyJson = vaultKey.asJson.deepMerge(Json.obj("privateKey" → pair.privkey.keyval.toPem.asJson))

    val req = HttpRequest(HttpMethods.POST, vaultAddr.withPath((Uri.Path.Empty / "v1" ++ Slash(vaultMount)) / pair.pubkey.id.value))
      .withEntity(legacyJson.noSpaces)
      .withHeaders(RawHeader("X-Vault-Token", vaultToken))

    val _http = Http()

    _http.singleRequest(req).futureValue.status shouldBe StatusCodes.NoContent

    vault.findKey(pair.pubkey.id).map(_.privateKey).futureValue shouldBe pair.privkey
  }

  def keySpecific(keyType: KeyType, name: String): Unit = {

    test("creates/finds a key " + name) {
      val keyPair = TufCrypto.generateKeyPair(keyType, keyType.crypto.defaultKeySize)

      vault.createKey(VaultKey(keyPair.pubkey.id, keyType, keyPair.pubkey, keyPair.privkey)).futureValue

      vault.findKey(keyPair.pubkey.id).map(_.privateKey).futureValue shouldBe keyPair.privkey
    }

    test("deletes a key " + name) {
      val keyPair = TufCrypto.generateKeyPair(keyType, keyType.crypto.defaultKeySize)

      vault.createKey(VaultKey(keyPair.pubkey.id, keyType, keyPair.pubkey, keyPair.privkey)).futureValue

      vault.deleteKey(keyPair.pubkey.id).futureValue

      vault.findKey(keyPair.pubkey.id).failed.futureValue shouldBe VaultResourceNotFound
    }
  }

  testsFor(keySpecific(RsaKeyType, "RSA"))
  testsFor(keySpecific(Ed25519KeyType, "Ed25519"))

}
