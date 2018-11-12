package com.advancedtelematic.libtuf.crypt

import java.io.{StringReader, StringWriter}
import java.security
import java.security.interfaces.RSAPublicKey
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{Signature ⇒ _, _}

import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.spec.ECParameterSpec
import com.advancedtelematic.libtuf.data.TufDataType.{ClientSignature, EcPrime256KeyType, EcPrime256TufKey, EcPrime256TufKeyPair, EcPrime256TufPrivateKey, Ed25519KeyType, Ed25519TufKey, Ed25519TufKeyPair, Ed25519TufPrivateKey, KeyId, KeyType, RSATufKey, RSATufKeyPair, RSATufPrivateKey, RsaKeyType, Signature, SignatureMethod, JsonSignedPayload, SignedPayload, TufKey, TufKeyPair, TufPrivateKey, ValidKeyId, ValidSignature}
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.openssl.jcajce.{JcaPEMKeyConverter, JcaPEMWriter}
import org.bouncycastle.util.encoders.{Base64, Hex}
import org.bouncycastle.openssl.{PEMKeyPair, PEMParser}
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry
import com.advancedtelematic.libtuf.data.TufDataType.SignatureMethod.SignatureMethod
import java.security.KeyFactory

import cats.data.{NonEmptyList, ValidatedNel}
import cats.implicits._
import io.circe.{Encoder, Json}
import io.circe.syntax._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.crypt.ECCrypto._
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TufRole}
import cats.implicits._

import scala.util.control.NoStackTrace
import scala.util.Try

trait TufCrypto[T <: KeyType] {
  def parsePublic(keyVal: String): Try[T#Pub]

  def parsePrivate(keyVal: String): Try[T#Priv]

  def encode(keyVal: T#Pub): Json

  def encode(keyVal: T#Priv): Json

  def generateKeyPair(keySize: Int): TufKeyPair = {
    require(validKeySize(keySize), "Key size too small")
    val keyPair = keyPairGenerator(keySize).generateKeyPair()
    toKeyPair(convertPublic(keyPair.getPublic), convertPrivate(keyPair.getPrivate))
  }

  def generateKeyPair(): TufKeyPair = generateKeyPair(defaultKeySize)

  def castToKeyPair(publicKey: TufKey, privateKey: TufPrivateKey): Try[TufKeyPair] = Try {
    toKeyPair(publicKey.asInstanceOf[T#Pub], privateKey.asInstanceOf[T#Priv])
  }

  def toKeyPair(publicKey: T#Pub, privateKey: T#Priv): TufKeyPair

  def parseKeyPair(publicKey: String, privateKey: String): Try[TufKeyPair] =
    for {
      priv ← parsePrivate(privateKey)
      pub ← parsePublic(publicKey)
    } yield toKeyPair(pub, priv)

  def convertPublic(publicKey: PublicKey): T#Pub

  def convertPrivate(privateKey: PrivateKey): T#Priv

  def signer: security.Signature

  def defaultKeySize: Int

  def validKeySize(size: Int): Boolean

  val signatureMethod: SignatureMethod

  def keyPairGenerator(keySize: Int): KeyPairGenerator
}

object TufCrypto {
  import SignedPayloadSignatureOps._

  case class SignatureMethodMismatch(fromKey: SignatureMethod, fromSig: SignatureMethod)
      extends Exception(s"SignatureMethod mismatch, The key is for $fromKey but the signature is for $fromSig")
      with NoStackTrace

  val rsaCrypto = new RsaCrypto

  val ed25519Crypto = new ED25519Crypto()

  lazy val ecPrime256Crypto = new ECPrime256Crypto()

  def signPayload(key: TufPrivateKey, payload: Json): Signature = {
    val bytes = payload.canonical.getBytes
    sign(key.keytype, key.keyval, bytes)
  }

  private def sign[T <: KeyType](keyType: T, privateKey: PrivateKey, data: Array[Byte]): Signature = {
    val signer = keyType.crypto.signer

    signer.initSign(privateKey)
    signer.update(data)
    val base64Sig = Base64.toBase64String(signer.sign())
    val validSignature = base64Sig.refineTry[ValidSignature].get

    Signature(validSignature, keyType.crypto.signatureMethod)
  }

  def convert[T <: KeyType](keyType: T, publicKey: PublicKey): T#Pub =
    keyType.crypto.convertPublic(publicKey)

  def isValid(signature: Signature, publicKey: PublicKey, data: Array[Byte]): Boolean = {
    val signer = signature.method match {
      case SignatureMethod.RSASSA_PSS_SHA256 ⇒ rsaCrypto.signer
      case SignatureMethod.ED25519 ⇒ ed25519Crypto.signer
      case other ⇒ throw new IllegalArgumentException(s"Unsupported signature method: $other")
    }
    val decodedSig = Base64.decode(signature.sig.value)
    signer.initVerify(publicKey)
    signer.update(data)
    signer.verify(decodedSig)
  }

  def isValid(signature: ClientSignature, publicKey: PublicKey, value: Json): Boolean = {
    val sig = Signature(signature.sig, signature.method)
    TufCrypto.isValid(sig, publicKey, value.canonical.getBytes)
  }

  implicit class PublicKeyOps(key: PublicKey) {
    val id: KeyId = {
      val publicKey = key.getEncoded
      val digest = new SHA256Digest()
      val buf = Array.fill[Byte](digest.getDigestSize)(0)
      digest.update(publicKey, 0, publicKey.length)
      digest.doFinal(buf, 0)
      Hex.toHexString(buf).refineTry[ValidKeyId].get
    }
  }

  implicit class KeyOps(key: Key) {
    def toPem = {
      val pemStrWriter = new StringWriter()
      val jcaPEMWriter = new JcaPEMWriter(pemStrWriter)
      jcaPEMWriter.writeObject(key)
      jcaPEMWriter.flush()
      pemStrWriter.toString
    }
  }

  def parsePublic[T <: KeyType](keyType: T, keyVal: String): Try[TufKey] =
    keyType.crypto.parsePublic(keyVal)

  def parsePrivate[T <: KeyType](keyType: T, keyVal: String): Try[TufPrivateKey] =
    keyType.crypto.parsePrivate(keyVal)

  def parsePublicPem(keyPem: String): Try[PublicKey] = Try {
    val parser = new PEMParser(new StringReader(keyPem))
    val converter = new JcaPEMKeyConverter()
    val pemKeyPair = parser.readObject().asInstanceOf[SubjectPublicKeyInfo]
    converter.getPublicKey(pemKeyPair)
  }

  def parsePrivatePem(keyPem: String): Try[PrivateKey] = Try {
    val parser = new PEMParser(new StringReader(keyPem))
    val converter = new JcaPEMKeyConverter()
    val pemKeyPair = parser.readObject().asInstanceOf[PEMKeyPair]
    converter.getPrivateKey(pemKeyPair.getPrivateKeyInfo)
  }

  def generateKeyPair[T <: KeyType](keyType: T, keySize: Int): TufKeyPair =
    keyType.crypto.generateKeyPair(keySize)

  def payloadSignatureIsValid[T : Encoder](rootRole: RootRole, signedPayload: SignedPayload[T])
                                          (implicit tufRole: TufRole[T]): ValidatedNel[String, SignedPayload[T]] = {
    val publicKeys = rootRole.keys.filterKeys(keyId => rootRole.roles(tufRole.roleType).keyids.contains(keyId))
    val threshold = rootRole.roles(tufRole.roleType).threshold
    payloadSignatureIsValid(publicKeys, threshold, signedPayload)
  }

  def payloadSignatureIsValid[T : Encoder](pubKeys: Map[KeyId, TufKey],
                                           threshold: Int,
                                           signedPayload: SignedPayload[T]): ValidatedNel[String, SignedPayload[T]] = {
    val sigsByKeyId = signedPayload.signatures.map(s => s.keyid -> s).toMap

    val validSignatureCount: ValidatedNel[String, List[KeyId]] =
      sigsByKeyId.par.map { case (keyId, sig) =>
        pubKeys.get(keyId)
          .toRight(s"key ${sig.keyid} required for role validation not found in authoritative role")
          .ensure(s"Invalid signature for key ${sig.keyid}") { key => signedPayload.isValidFor(key) }
          .map(_.id)
          .toValidatedNel
      }.toList.sequence

    validSignatureCount.ensure(NonEmptyList.of(s"Valid signature count must be >= threshold ($threshold)")) { validSignatures =>
      validSignatures.size >= threshold && threshold > 0
    }.map(_ => signedPayload)
  }
}

protected [crypt] object ECCrypto {
  sealed trait CurveId {
    def parameterSpec: ECParameterSpec
  }
  case object Curve25519 extends CurveId {
    override def parameterSpec: ECParameterSpec = {
      val ecP = ECNamedCurveTable.getParameterSpec("curve25519")
      new ECParameterSpec(ecP.getCurve, ecP.getG, ecP.getN, ecP.getH, ecP.getSeed)
    }
  }
  case object Prime256v1 extends CurveId {
    override def parameterSpec: ECParameterSpec = ECNamedCurveTable.getParameterSpec("prime256v1")
  }

  protected [crypt] class SignatureAlgorithm(val method: SignatureMethod, val id: String)
  case object SHA256withECDSA extends SignatureAlgorithm(SignatureMethod.ED25519, "SHA256withECDSA")
  case object SHA512withECDSA extends SignatureAlgorithm(SignatureMethod.ECPrime256V1, "SHA512withECDSA")
}

protected [crypt] abstract class ECCrypto[T <: KeyType](curve: CurveId, signatureAlgorithm: SignatureAlgorithm) extends TufCrypto[T] {
  private lazy val fac = KeyFactory.getInstance("ECDSA", "BC")

  private lazy val generator = {
    val generator = KeyPairGenerator.getInstance("ECDSA", "BC")
    generator.initialize(curve.parameterSpec)
    generator
  }

  override def parsePublic(publicKeyHex: String): Try[T#Pub] = Try {
    val spec = new X509EncodedKeySpec(Hex.decode(publicKeyHex))
    convertPublic(fac.generatePublic(spec))
  }

  override def parsePrivate(privateKeyHex: String): Try[T#Priv] = Try {
    val spec = new PKCS8EncodedKeySpec(Hex.decode(privateKeyHex))
    convertPrivate(fac.generatePrivate(spec))
  }

  def keyPairGenerator(keySize: Int): KeyPairGenerator = generator

  override def encode(keyVal: T#Pub): Json = Json.fromString(Hex.toHexString(keyVal.keyval.getEncoded))

  override def encode(keyVal: T#Priv): Json = Json.fromString(Hex.toHexString(keyVal.keyval.getEncoded))

  override def signer: security.Signature = java.security.Signature.getInstance(signatureAlgorithm.id, "BC")

  override val signatureMethod: SignatureMethod = signatureAlgorithm.method

  override def validKeySize(size: Int): Boolean = size == 256

  override def defaultKeySize: Int = 256
}

protected [crypt] class ED25519Crypto extends ECCrypto[Ed25519KeyType.type](Curve25519, SHA256withECDSA) {
  override def toKeyPair(publicKey: Ed25519TufKey, privateKey: Ed25519TufPrivateKey): TufKeyPair =
    Ed25519TufKeyPair(publicKey, privateKey)

  override def convertPrivate(privateKey: PrivateKey): Ed25519TufPrivateKey = Ed25519TufPrivateKey(privateKey)

  override def convertPublic(publicKey: PublicKey): Ed25519TufKey = Ed25519TufKey(publicKey)
}

protected [crypt] class ECPrime256Crypto extends ECCrypto[EcPrime256KeyType.type](Prime256v1, SHA512withECDSA) {
  override def toKeyPair(publicKey: EcPrime256TufKey, privateKey: EcPrime256TufPrivateKey): TufKeyPair =
    EcPrime256TufKeyPair(publicKey, privateKey)

  override def convertPrivate(privateKey: PrivateKey): EcPrime256TufPrivateKey = EcPrime256TufPrivateKey(privateKey)

  override def convertPublic(publicKey: PublicKey): EcPrime256TufKey = EcPrime256TufKey(publicKey)
}


protected [crypt] class RsaCrypto extends TufCrypto[RsaKeyType.type] {

  import TufCrypto.KeyOps

  override def parsePublic(publicKey: String): Try[RSATufKey] = {
    val parser = new PEMParser(new StringReader(publicKey))
    val converter = new JcaPEMKeyConverter()

    Try {
      val pemKeyPair = parser.readObject().asInstanceOf[SubjectPublicKeyInfo]
      val pubKey = converter.getPublicKey(pemKeyPair)
      pubKey match {
        case rsaPubKey: RSAPublicKey if rsaPubKey.getModulus.bitLength() >= 2048 => convertPublic(pubKey)
        case _: RSAPublicKey => throw new IllegalArgumentException("Key size too small, must be >= 2048")
        case _ => throw new IllegalArgumentException("Key is not an RSAPublicKey")
      }
    }
  }

  override def parsePrivate(privateKey: String): Try[RSATufPrivateKey] = {
    val parser = new PEMParser(new StringReader(privateKey))
    val converter = new JcaPEMKeyConverter()

    Try {
      val pemKeyPair = parser.readObject().asInstanceOf[PEMKeyPair]
      convertPrivate(converter.getKeyPair(pemKeyPair).getPrivate)
    }
  }

  override def encode(keyVal: RSATufKey): Json = Json.fromString(keyVal.keyval.toPem)

  override def encode(keyVal: RSATufPrivateKey): Json = Json.fromString(keyVal.keyval.toPem)

  override def signer: security.Signature = java.security.Signature.getInstance("SHA256withRSAandMGF1", "BC") // RSASSA-PSS

  override val signatureMethod: SignatureMethod = SignatureMethod.RSASSA_PSS_SHA256

  override def convertPublic(publicKey: PublicKey): RSATufKey = RSATufKey(publicKey)

  override def convertPrivate(privateKey: PrivateKey): RSATufPrivateKey = RSATufPrivateKey(privateKey)

  override def toKeyPair(publicKey: RSATufKey, privateKey: RSATufPrivateKey): TufKeyPair = RSATufKeyPair(publicKey, privateKey)

  override def validKeySize(size: Int): Boolean = size >= 2048

  override def defaultKeySize: Int = 2048

  override def keyPairGenerator(keySize: Int): KeyPairGenerator = {
    val keyGen = KeyPairGenerator.getInstance("RSA", "BC")
    keyGen.initialize(keySize)
    keyGen
  }
}
