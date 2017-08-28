package com.advancedtelematic.libtuf.data

import java.security.{PrivateKey, PublicKey}
import java.util.UUID

import akka.http.scaladsl.server.PathMatchers
import akka.http.scaladsl.unmarshalling.Unmarshaller
import cats.Show
import com.advancedtelematic.libats.codecs.CirceEnum
import com.advancedtelematic.libats.data.UUIDKey.{UUIDKey, UUIDKeyObj}
import com.advancedtelematic.libats.slick.codecs.SlickEnum
import com.advancedtelematic.libats.messaging_datatype.DataType.HashMethod.HashMethod
import com.advancedtelematic.libats.messaging_datatype.DataType.ValidChecksum
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType.SignatureMethod.SignatureMethod
import eu.timepit.refined.api.{Refined, Validate}
import io.circe.Encoder
import TufCrypto.PublicKeyOps
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry

import scala.util.Try

object TufDataType {
  final case class ValidHardwareIdentifier()
  type HardwareIdentifier = Refined[String, ValidHardwareIdentifier]
  implicit val validHardwareIdentifier: Validate.Plain[String, ValidHardwareIdentifier] = ValidationUtils.validInBetween(min = 0, max = 200, ValidHardwareIdentifier())

  object TargetFormat extends CirceEnum with SlickEnum {
    type TargetFormat = Value

    val OSTREE, BINARY = Value

    implicit val targetFormatFromStringUnmarshaller = Unmarshaller.strict[String, TargetFormat](s => this.withName(s.toUpperCase))
  }

  case class TargetName(value: String) extends AnyVal
  case class TargetVersion(value: String) extends AnyVal

  case class Checksum(method: HashMethod, hash: Refined[String, ValidChecksum])
  case class ValidKeyId()
  type KeyId = Refined[String, ValidKeyId]
  implicit val validKeyId: Validate.Plain[String, ValidKeyId] =
    ValidationUtils.validHexValidation(ValidKeyId(), length = 64)
  val KeyIdPath = PathMatchers.Segment.flatMap(_.refineTry[ValidKeyId].toOption)

  case class ValidSignature()
  case class Signature(sig: Refined[String, ValidSignature], method: SignatureMethod = SignatureMethod.RSASSA_PSS)
  implicit val validSignature: Validate.Plain[String, ValidSignature] =
    ValidationUtils.validBase64Validation(ValidSignature())

  object RoleType extends Enumeration with SlickEnum {
    type RoleType = Value

    val ROOT, SNAPSHOT, TARGETS, TIMESTAMP = Value

    val ALL = List(ROOT, SNAPSHOT, TARGETS, TIMESTAMP)

    implicit val show = Show.show[Value](_.toString.toLowerCase)

    val Path = PathMatchers.Segment.flatMap(v => Try(withName(v.toUpperCase)).toOption)

    val JsonRoleTypeMetaPath = PathMatchers.Segment.flatMap { str =>
      val (roleTypeStr, _) = str.splitAt(str.indexOf(".json"))
      Try(RoleType.withName(roleTypeStr.toUpperCase)).toOption
    }
  }

  object SignatureMethod extends CirceEnum {
    type SignatureMethod = Value

    val RSASSA_PSS = Value("rsassa-pss")

    val ED25519 = Value("ed25519")
  }

  case class RepoId(uuid: UUID) extends UUIDKey
  object RepoId extends UUIDKeyObj[RepoId]

  case class ClientSignature(keyid: KeyId, method: SignatureMethod, sig: Refined[String, ValidSignature]) {
    def toSignature: Signature = Signature(sig, method)
  }

  implicit class SignatureToClientSignatureOps(value: Signature) {
    def toClient(keyId: KeyId): ClientSignature =
      ClientSignature(keyId, value.method, value.sig)
  }

  case class SignedPayload[T : Encoder](signatures: Seq[ClientSignature], signed: T)

  sealed trait KeyType {
    type Pub <: TufKey
    type Priv <: TufPrivateKey

    val crypto: TufCrypto[this.type]
  }
  case object RsaKeyType extends KeyType {
    type Pub = RSATufKey
    type Priv = RSATufPrivateKey

    val crypto = TufCrypto.rsaCrypto
  }
  case object EdKeyType extends KeyType {
    type Pub = EdTufKey
    type Priv = EdTufPrivateKey

    val crypto = TufCrypto.edCrypto
  }

  sealed trait TufKey {
    val keyval: PublicKey
    lazy val id = keyval.id
    def keytype: KeyType
  }
  case class RSATufKey(override val keyval: PublicKey) extends TufKey {
    override def keytype: KeyType = RsaKeyType
  }
  case class EdTufKey(override val keyval: PublicKey) extends TufKey {
    override def keytype: KeyType = EdKeyType
  }

  sealed trait TufPrivateKey {
    val keyval: PrivateKey
    def keytype: KeyType
  }
  case class RSATufPrivateKey(override val keyval: PrivateKey) extends TufPrivateKey {
    override def keytype: KeyType = RsaKeyType
  }
  case class EdTufPrivateKey(override val keyval: PrivateKey) extends TufPrivateKey {
    override def keytype: KeyType = EdKeyType
  }
}
