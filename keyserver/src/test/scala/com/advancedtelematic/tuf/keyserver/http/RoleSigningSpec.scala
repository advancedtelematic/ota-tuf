package com.advancedtelematic.tuf.keyserver.http

import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import io.circe.syntax._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultKey
import com.advancedtelematic.libtuf.crypt.TufCrypto._
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, KeyType, RsaKeyType, Signature}
import org.bouncycastle.util.encoders.Base64
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.ExecutionContext

case class TestPayload(propertyB: String = "some B", propertyA: String = "some A",
                       arrayMapProp: List[Map[String, Int]] = List(Map("bbb" -> 1, "aaa" -> 0)),
                       mapProp: Map[String, Int] = Map("bb" -> 1, "aa" -> 0))

class RoleSigningSpec extends TufKeyserverSpec with DatabaseSpec with PatienceConfiguration {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(3, Seconds))

  implicit val encoder = io.circe.generic.semiauto.deriveEncoder[TestPayload]
  implicit val decoder = io.circe.generic.semiauto.deriveDecoder[TestPayload]

  def keySpecific(keyType: KeyType, name: String): Unit = {

    val keyPair = keyType.crypto.generateKeyPair(keyType.crypto.defaultKeySize)
    val dbKey = Key(keyPair.pubkey.id, RoleId.generate(), RsaKeyType, keyPair.pubkey.keyval)

    lazy val roleSigning = {
      fakeVault.createKey(VaultKey(dbKey.id, dbKey.keyType, keyPair.pubkey, keyPair.privkey)).futureValue
      new RoleSigning(fakeVault)
    }

    test("signs a payload with a key " + name) {
      val payload = TestPayload()

      val signature = roleSigning.signForClient(payload)(dbKey).futureValue

      new String(Base64.decode(signature.sig.value)) shouldBe a[String]
    }

    test("generates valid signatures " + name) {
      val payload = TestPayload()

      val clientSignature = roleSigning.signForClient(payload)(dbKey).futureValue
      val signature = Signature(clientSignature.sig, clientSignature.method)

      TufCrypto.isValid(signature, keyPair.pubkey.keyval, payload.asJson.canonical.getBytes) shouldBe true
    }

    test("generates valid signatures when verifying with canonical representation " + name) {
      val payload = TestPayload()

      val clientSignature = roleSigning.signForClient(payload)(dbKey).futureValue
      val signature = Signature(clientSignature.sig, clientSignature.method)

      val canonicalJson = """{"arrayMapProp":[{"aaa":0,"bbb":1}],"mapProp":{"aa":0,"bb":1},"propertyA":"some A","propertyB":"some B"}""".getBytes

      TufCrypto.isValid(signature, keyPair.pubkey.keyval, canonicalJson) shouldBe true
    }

  }

  testsFor(keySpecific(RsaKeyType, "RSA"))
  testsFor(keySpecific(Ed25519KeyType, "Ed25519"))

}
