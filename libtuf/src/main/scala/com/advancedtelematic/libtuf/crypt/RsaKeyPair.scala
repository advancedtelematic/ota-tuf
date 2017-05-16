package com.advancedtelematic.libtuf.crypt

import java.io.{StringReader, StringWriter}
import java.security.{Signature => _, _}

import cats.Show
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, Signature, ValidKeyId, ValidSignature}
import com.advancedtelematic.libtuf.data.TufDataType.SignatureMethod
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.openssl.jcajce.{JcaPEMKeyConverter, JcaPEMWriter}
import org.bouncycastle.util.encoders.{Base64, Hex}
import org.bouncycastle.openssl.{PEMKeyPair, PEMParser}
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry
import scala.util.Try

object RsaKeyPair {
  def generate(size: Int = 2048): KeyPair = {
    if(size < 2048) {
      throw new IllegalArgumentException("Key size too small, must be >= 2048")
    }
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
    val base64Sig = Base64.toBase64String(signature)
    val sig = base64Sig.refineTry[ValidSignature].get
    Signature(sig, SignatureMethod.RSASSA_PSS)
  }

  def isValid(publicKey: PublicKey, signature: Signature, data: Array[Byte]): Boolean = {
    if (signature.method != SignatureMethod.RSASSA_PSS)
      throw new IllegalArgumentException(s"Signature method not supported: ${signature.method}")

    val signer = java.security.Signature.getInstance("SHA256withRSAandMGF1", "BC") // RSASSA-PSS
    val decodedSig = Base64.decode(signature.sig.value)
    signer.initVerify(publicKey)
    signer.update(data)
    signer.verify(decodedSig)
  }

  implicit def keyShow[T <: java.security.Key]: Show[T] = Show.show { key =>
    val pemStrWriter = new StringWriter()
    val jcaPEMWriter = new JcaPEMWriter(pemStrWriter)
    jcaPEMWriter.writeObject(key)
    jcaPEMWriter.flush()
    pemStrWriter.toString
  }

  implicit class RsaPublicKeyOps(key: PublicKey) {
    def id: KeyId = {
      val publicKey = key.getEncoded
      val digest = new SHA256Digest()
      val buf = Array.fill[Byte](digest.getDigestSize)(0)
      digest.update(publicKey, 0, publicKey.length)
      digest.doFinal(buf, 0)
      Hex.toHexString(buf).refineTry[ValidKeyId].get
    }
  }

  implicit class RsaKeyIdConversion(keyPair: KeyPair) {
    def id: KeyId = keyPair.getPublic.id
  }
}
