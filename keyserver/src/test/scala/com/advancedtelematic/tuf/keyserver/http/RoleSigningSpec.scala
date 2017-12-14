package com.advancedtelematic.tuf.keyserver.http

import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import io.circe.syntax._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultKey
import com.advancedtelematic.libtuf.crypt.TufCrypto._
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType.{Ec25519KeyType, Ec25519TufKeyPair, RSATufKeyPair, RsaKeyType, Signature}
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

  val RSATufKeyPair(publicKey, privateKey) = TufCrypto.generateKeyPair(RsaKeyType, 2048)

  val dbKey = Key(publicKey.id, RoleId.generate(), RsaKeyType, publicKey.keyval)

  implicit val encoder = io.circe.generic.semiauto.deriveEncoder[TestPayload]
  implicit val decoder = io.circe.generic.semiauto.deriveDecoder[TestPayload]

  lazy val roleSigning = {
    fakeVault.createKey(VaultKey(dbKey.id, dbKey.keyType, publicKey, privateKey)).futureValue
    new RoleSigning(fakeVault)
  }

  test("signs a payload with a key") {
    val payload = TestPayload()

    val signature = roleSigning.signForClient(payload)(dbKey).futureValue

    new String(Base64.decode(signature.sig.value)) shouldBe a[String]
  }

  test("generates valid signatures")  {
    val payload = TestPayload()

    val clientSignature = roleSigning.signForClient(payload)(dbKey).futureValue
    val signature = Signature(clientSignature.sig, clientSignature.method)

    TufCrypto.isValid(signature, publicKey.keyval, payload.asJson.canonical.getBytes) shouldBe true
  }

  test("generates valid ed25519 signatures")  {
    val payload = TestPayload()

    val Ec25519TufKeyPair(publicKey, privateKey) = TufCrypto.generateKeyPair(Ec25519KeyType, 256)
    val dbKey = Key(publicKey.id, RoleId.generate(), Ec25519KeyType, publicKey.keyval)
    fakeVault.createKey(VaultKey(dbKey.id, dbKey.keyType, publicKey, privateKey)).futureValue

    val clientSignature = roleSigning.signForClient(payload)(dbKey).futureValue
    val signature = Signature(clientSignature.sig, clientSignature.method)

    TufCrypto.isValid(signature, publicKey.keyval, payload.asJson.canonical.getBytes) shouldBe true
  }


  test("generates valid signatures when verifying with canonical representation") {
    val payload = TestPayload()

    val clientSignature = roleSigning.signForClient(payload)(dbKey).futureValue
    val signature = Signature(clientSignature.sig, clientSignature.method)

    val canonicalJson = """{"arrayMapProp":[{"aaa":0,"bbb":1}],"mapProp":{"aa":0,"bb":1},"propertyA":"some A","propertyB":"some B"}""".getBytes

    TufCrypto.isValid(signature, publicKey.keyval, canonicalJson) shouldBe true
  }
}
