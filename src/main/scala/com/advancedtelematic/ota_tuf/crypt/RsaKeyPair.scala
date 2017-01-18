package com.advancedtelematic.ota_tuf.crypt

import java.io.{StringReader, StringWriter}
import java.security.{Signature => _, _}

import cats.Show
import com.advancedtelematic.ota_tuf.data.DataType.KeyId
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.openssl.jcajce.{JcaPEMKeyConverter, JcaPEMWriter}
import org.bouncycastle.util.encoders.Hex
import org.bouncycastle.openssl.{PEMKeyPair, PEMParser}
import com.advancedtelematic.ota_tuf.data.DataType._
import com.advancedtelematic.ota_tuf.data.SignatureMethod
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
import com.advancedtelematic.ota_tuf.data.RefinedUtils._

import scala.util.Try

object RsaKeyPair {
  def generate(size: Int = 512): KeyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA", "BC")
    keyGen.initialize(size, new SecureRandom())
    keyGen.generateKeyPair()
  }

  def parsePublic(publicKey: String): Try[PublicKey] = {
    val parser = new PEMParser(new StringReader(publicKey))
    val converter = new JcaPEMKeyConverter()

    Try {
      val pemKeyPair = parser.readObject().asInstanceOf[SubjectPublicKeyInfo]
      converter.getPublicKey(pemKeyPair)
    }
  }

  def parseKeyPair(privateKey: String): Try[KeyPair] = {
    val parser = new PEMParser(new StringReader(privateKey))
    val converter = new JcaPEMKeyConverter()

    Try {
      val pemKeyPair = parser.readObject().asInstanceOf[PEMKeyPair]
      converter.getKeyPair(pemKeyPair)
    }
  }

  def sign(privateKey: PrivateKey, data: Array[Byte]): Signature = {
    val signer = java.security.Signature.getInstance("SHA256withRSAandMGF1", "BC") // RSASSA-PSS
    signer.initSign(privateKey)
    signer.update(data)
    val signature = signer.sign()
    val hexSignature = Hex.toHexString(signature)
    val sig = refineTry[String, ValidSignature](hexSignature).get
    Signature(sig, SignatureMethod.RSASSA_PSS)
  }

  def isValid(publicKey: PublicKey, signature: Signature, data: Array[Byte]): Boolean = {
    if (signature.method != SignatureMethod.RSASSA_PSS)
      throw new IllegalArgumentException(s"Signature method not supported: ${signature.method}")

    val signer = java.security.Signature.getInstance("SHA256withRSAandMGF1", "BC") // RSASSA-PSS
    val hexDecodedSignature = Hex.decode(signature.hex.get)
    signer.initVerify(publicKey)
    signer.update(data)
    signer.verify(hexDecodedSignature)
  }

  implicit def keyShow[T <: java.security.Key]: Show[T] = Show.show { key ⇒
    val pemStrWriter = new StringWriter()
    val jcaPEMWriter = new JcaPEMWriter(pemStrWriter)
    jcaPEMWriter.writeObject(key)
    jcaPEMWriter.flush()
    pemStrWriter.toString
  }

  implicit class RsaKeyIdConversion(keyPair: KeyPair) {
    def id: KeyId = {
      val publicKey = keyPair.getPublic.getEncoded
      val digest = new SHA256Digest()
      val buf = Array.fill[Byte](digest.getDigestSize)(0)
      digest.update(publicKey, 0, publicKey.length)
      digest.doFinal(buf, 0)
      refineTry[String, ValidKeyId](Hex.toHexString(buf)).get
    }
  }
}
