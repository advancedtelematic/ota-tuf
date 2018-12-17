package com.advancedtelematic.libtuf.crypt

import java.io.{StringReader, StringWriter}
import java.security
import java.security.interfaces.RSAPublicKey
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{KeyFactory, MessageDigest, Signature => _, _}

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.implicits._
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.crypt.ECCrypto._
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TufRole}
import com.advancedtelematic.libtuf.data.TufDataType.SignatureMethod.SignatureMethod
import com.advancedtelematic.libtuf.data.TufDataType.{ClientSignature, EcPrime256KeyType, EcPrime256TufKey, EcPrime256TufKeyPair, EcPrime256TufPrivateKey, Ed25519KeyType, Ed25519TufKey, Ed25519TufKeyPair, Ed25519TufPrivateKey, KeyId, KeyType, RSATufKey, RSATufKeyPair, RSATufPrivateKey, RsaKeyType, Signature, SignatureMethod, SignedPayload, TufKey, TufKeyPair, TufPrivateKey, ValidKeyId, ValidSignature}
import io.circe.{Encoder, Json}
import net.i2p.crypto.eddsa.spec.EdDSANamedCurveTable
import net.i2p.crypto.eddsa.{EdDSAEngine, EdDSAPrivateKey, EdDSAPublicKey}
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.spec.ECParameterSpec
import org.bouncycastle.openssl.jcajce.{JcaPEMKeyConverter, JcaPEMWriter}
import org.bouncycastle.openssl.{PEMKeyPair, PEMParser}
import org.bouncycastle.util.encoders.{Base64, Hex}

import scala.util.Try
import scala.util.control.NoStackTrace

trait TufCrypto[T <: KeyType] {
  def parsePublic(keyVal: String): Try[T#Pub]

  def parsePrivate(keyVal: String): Try[T#Priv]

  def encode(keyVal: T#Pub): Json

  def encode(keyVal: T#Priv): Json

  def generateKeyPair(keySize: Int): TufKeyPair

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

  def signer: security.Signature

  def defaultKeySize: Int

  def validKeySize(size: Int): Boolean

  val signatureMethod: SignatureMethod
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

  def isValid(signature: Signature, publicKey: TufKey, data: Array[Byte]): Boolean = {
    val signer = signature.method match {
      case SignatureMethod.RSASSA_PSS_SHA256 ⇒ rsaCrypto.signer
      case SignatureMethod.ED25519 ⇒ ed25519Crypto.signer
      case other ⇒ throw new IllegalArgumentException(s"Unsupported signature method: $other")
    }
    val decodedSig = Base64.decode(signature.sig.value)
    signer.initVerify(publicKey.keyval)
    signer.update(data)
    signer.verify(decodedSig)
  }

  def isValid(signature: ClientSignature, publicKey: TufKey, value: Json): Boolean = {
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

    val validSignatures: List[ValidatedNel[String, KeyId]] =
      sigsByKeyId.par.map { case (keyId, sig) =>
        pubKeys.get(keyId)
          .toRight(s"key ${sig.keyid} required for role validation not found in authoritative role")
          .ensure(s"Invalid signature for key ${sig.keyid}") { key => signedPayload.isValidFor(key) }
          .map(_.id)
          .toValidatedNel
      }.toList

    val validSignatureCount = validSignatures.count(_.isValid)

    if(validSignatureCount >= threshold && threshold > 0) {
      Valid(signedPayload)
    } else {
      validSignatures.sequence_ match {
        case Valid(_) =>
          Invalid(s"Valid signature count must be >= threshold ($threshold)").toValidatedNel
        case Invalid(errors) =>
          Invalid(errors)
      }
    }
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

protected [crypt] class ECPrime256Crypto extends TufCrypto[EcPrime256KeyType.type] {
  private lazy val fac = KeyFactory.getInstance("ECDSA", "BC")

  private lazy val generator = {
    val generator = KeyPairGenerator.getInstance("ECDSA", "BC")
    generator.initialize(Prime256v1.parameterSpec)
    generator
  }

  override def parsePublic(publicKeyHex: String): Try[EcPrime256TufKey] = Try {
    val spec = new X509EncodedKeySpec(Hex.decode(publicKeyHex))
    EcPrime256TufKey(fac.generatePublic(spec))
  }

  override def parsePrivate(privateKeyHex: String): Try[EcPrime256TufPrivateKey] = Try {
    val spec = new PKCS8EncodedKeySpec(Hex.decode(privateKeyHex))
    EcPrime256TufPrivateKey(fac.generatePrivate(spec))
  }

  def keyPairGenerator(keySize: Int): KeyPairGenerator = generator

  override def encode(keyVal: EcPrime256TufKey): Json = Json.fromString(Hex.toHexString(keyVal.keyval.getEncoded))

  override def encode(keyVal: EcPrime256TufPrivateKey): Json = Json.fromString(Hex.toHexString(keyVal.keyval.getEncoded))

  override def signer: security.Signature = java.security.Signature.getInstance(SHA512withECDSA.id, "BC")

  override val signatureMethod: SignatureMethod = SHA512withECDSA.method

  override def validKeySize(size: Int): Boolean = size == 256

  override def defaultKeySize: Int = 256

  override def toKeyPair(publicKey: EcPrime256TufKey, privateKey: EcPrime256TufPrivateKey): TufKeyPair =
    EcPrime256TufKeyPair(publicKey, privateKey)

  override def generateKeyPair(keySize: Int): TufKeyPair = {
    require(validKeySize(keySize), "Key size too small")
    val keyPair = keyPairGenerator(keySize).generateKeyPair()
    EcPrime256TufKeyPair(EcPrime256TufKey(keyPair.getPublic), EcPrime256TufPrivateKey(keyPair.getPrivate))
  }
}

protected [crypt] class ED25519Crypto extends TufCrypto[Ed25519KeyType.type] {
  private lazy val keyPairGenerator = new net.i2p.crypto.eddsa.KeyPairGenerator()

  override def toKeyPair(publicKey: Ed25519TufKey, privateKey: Ed25519TufPrivateKey): TufKeyPair =
    Ed25519TufKeyPair(publicKey, privateKey)

  override def parsePublic(publicKeyHex: String): Try[Ed25519TufKey] = Try {
    val spec = new X509EncodedKeySpec(Base64.decode(publicKeyHex))
    Ed25519TufKey(new EdDSAPublicKey(spec))
  }

  override def parsePrivate(privateKeyHex: String): Try[Ed25519TufPrivateKey] = Try {
    val spec = new PKCS8EncodedKeySpec(Base64.decode(privateKeyHex))
    Ed25519TufPrivateKey(new EdDSAPrivateKey(spec))
  }

  override def encode(keyVal: Ed25519TufKey): Json =
    Json.fromString(Base64.toBase64String(keyVal.keyval.getEncoded))

  override def encode(keyVal: Ed25519TufPrivateKey): Json =
    Json.fromString(Base64.toBase64String(keyVal.keyval.getEncoded))

  override def signer: security.Signature = {
    val spec = EdDSANamedCurveTable.getByName(EdDSANamedCurveTable.ED_25519)
    new EdDSAEngine(MessageDigest.getInstance(spec.getHashAlgorithm))
  }

  override def defaultKeySize: Int = 256

  override def validKeySize(size: Int): Boolean = size == 256

  override val signatureMethod: SignatureMethod = SignatureMethod.ED25519

  override def generateKeyPair(keySize: Int): TufKeyPair = {
    require(validKeySize(keySize), "Key size too small")
    val keyPair = keyPairGenerator.generateKeyPair()
    Ed25519TufKeyPair(Ed25519TufKey(keyPair.getPublic), Ed25519TufPrivateKey(keyPair.getPrivate))
  }
}


protected [crypt] class RsaCrypto extends TufCrypto[RsaKeyType.type] {

  override def parsePublic(publicKey: String): Try[RSATufKey] = {
    val parser = new PEMParser(new StringReader(publicKey))
    val converter = new JcaPEMKeyConverter()

    Try {
      val pemKeyPair = parser.readObject().asInstanceOf[SubjectPublicKeyInfo]
      val pubKey = converter.getPublicKey(pemKeyPair)
      pubKey match {
        case rsaPubKey: RSAPublicKey if rsaPubKey.getModulus.bitLength() >= 2048 => RSATufKey(pubKey)
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
      RSATufPrivateKey(converter.getKeyPair(pemKeyPair).getPrivate)
    }
  }

  private def toPem(key: Key): String = {
    val pemStrWriter = new StringWriter()
    val jcaPEMWriter = new JcaPEMWriter(pemStrWriter)
    jcaPEMWriter.writeObject(key)
    jcaPEMWriter.flush()
    pemStrWriter.toString
  }

  override def encode(keyVal: RSATufKey): Json = Json.fromString(toPem(keyVal.keyval))

  override def encode(keyVal: RSATufPrivateKey): Json = Json.fromString(toPem(keyVal.keyval))

  override def signer: security.Signature = java.security.Signature.getInstance("SHA256withRSAandMGF1", "BC") // RSASSA-PSS

  override val signatureMethod: SignatureMethod = SignatureMethod.RSASSA_PSS_SHA256

  override def toKeyPair(publicKey: RSATufKey, privateKey: RSATufPrivateKey): TufKeyPair = RSATufKeyPair(publicKey, privateKey)

  override def validKeySize(size: Int): Boolean = size >= 2048

  override def defaultKeySize: Int = 2048

  private def keyPairGenerator(keySize: Int): KeyPairGenerator = {
    val keyGen = KeyPairGenerator.getInstance("RSA", "BC")
    keyGen.initialize(keySize)
    keyGen
  }

  override def generateKeyPair(keySize: Int): TufKeyPair = {
    require(validKeySize(keySize), "Key size too small")
    val keyPair = keyPairGenerator(keySize).generateKeyPair()
    RSATufKeyPair(RSATufKey(keyPair.getPublic), RSATufPrivateKey(keyPair.getPrivate))
  }
}
