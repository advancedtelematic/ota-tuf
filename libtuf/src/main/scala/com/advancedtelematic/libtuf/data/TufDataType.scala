package com.advancedtelematic.libtuf.data

import java.util.UUID

import akka.http.scaladsl.server.PathMatchers
import cats.Show
import com.advancedtelematic.libats.codecs.CirceEnum
import com.advancedtelematic.libats.data.UUIDKey.{UUIDKey, UUIDKeyObj}
import com.advancedtelematic.libats.slick.codecs.SlickEnum
import com.advancedtelematic.libats.messaging_datatype.DataType.HashMethod.HashMethod
import com.advancedtelematic.libats.messaging_datatype.DataType.ValidChecksum
import com.advancedtelematic.libtuf.data.TufDataType.SignatureMethod.SignatureMethod
import eu.timepit.refined.api.{Refined, Validate}
import io.circe.{Decoder, Encoder}

import scala.util.Try

object TufDataType {
  // TODO: Move to libats/libtuf?
  final case class ValidHardwareIdentifier()
  type HardwareIdentifier = Refined[String, ValidHardwareIdentifier]
  implicit val validHardwareIdentifier: Validate.Plain[String, ValidHardwareIdentifier] = ValidationUtils.validInBetween(min = 0, max = 200, ValidHardwareIdentifier())

  case class TargetName(value: String) extends AnyVal
  case class TargetVersion(value: String) extends AnyVal

  case class Checksum(method: HashMethod, hash: Refined[String, ValidChecksum])

  case class ValidKeyId()
  type KeyId = Refined[String, ValidKeyId]
  implicit val validKeyId: Validate.Plain[String, ValidKeyId] =
    ValidationUtils.validHexValidation(ValidKeyId(), length = 64)

  case class ValidSignature()
  case class Signature(sig: Refined[String, ValidSignature], method: SignatureMethod = SignatureMethod.RSASSA_PSS)
  implicit val validSignature: Validate.Plain[String, ValidSignature] =
    ValidationUtils.validBase64Validation(ValidSignature())

  object KeyType extends CirceEnum with SlickEnum {
    type KeyType = Value

    val RSA = Value
  }

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
}
