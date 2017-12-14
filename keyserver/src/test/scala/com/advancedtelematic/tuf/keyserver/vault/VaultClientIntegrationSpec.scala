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
import com.advancedtelematic.libtuf.data.TufDataType.{Ec25519KeyType, RSATufKeyPair, RsaKeyType}
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

  test("creates/finds a key") {
    val RSATufKeyPair(publicKey, privateKey) = TufCrypto.generateKeyPair(RsaKeyType, 2048)

    vault.createKey(VaultKey(publicKey.id, RsaKeyType, publicKey, privateKey)).futureValue

    vault.findKey(publicKey.id).map(_.privateKey).futureValue shouldBe privateKey
  }

  test("deletes a key") {
    val RSATufKeyPair(publicKey, privateKey) = TufCrypto.generateKeyPair(RsaKeyType, 2048)

    vault.createKey(VaultKey(publicKey.id, RsaKeyType, publicKey, privateKey)).futureValue

    vault.deleteKey(publicKey.id).futureValue

    vault.findKey(publicKey.id).failed.futureValue shouldBe VaultResourceNotFound
  }

  test("renews the token") {
    vault.renewToken().futureValue shouldBe(())
  }

  test("can store ed25519 key") {
    val pair = TufCrypto.generateKeyPair(Ec25519KeyType, 256)

    vault.createKey(VaultKey(pair.pubkey.id, Ec25519KeyType, pair.pubkey, pair.privkey)).futureValue

    vault.findKey(pair.pubkey.id).map(_.privateKey).futureValue.keyval.getEncoded shouldBe pair.privkey.keyval.getEncoded
  }

  test("can decode a legacy key") {
    val pair = TufCrypto.generateKeyPair(RsaKeyType, 2048)

    val vaultKey = VaultKey(pair.pubkey.id, Ec25519KeyType, pair.pubkey, pair.privkey)

    val legacyJson = vaultKey.asJson.deepMerge(Json.obj("privateKey" → pair.privkey.keyval.toPem.asJson))

    val req = HttpRequest(HttpMethods.POST, vaultAddr.withPath((Uri.Path.Empty / "v1" ++ Slash(vaultMount)) / pair.pubkey.id.value))
      .withEntity(legacyJson.noSpaces)
      .withHeaders(RawHeader("X-Vault-Token", vaultToken))

    val _http = Http()

    _http.singleRequest(req).futureValue.status shouldBe StatusCodes.NoContent

    vault.findKey(pair.pubkey.id).map(_.privateKey).futureValue shouldBe pair.privkey
  }
}
