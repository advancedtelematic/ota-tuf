package com.advancedtelematic.libtuf.data

import akka.http.scaladsl.server.PathMatchers
import cats.Show
import com.advancedtelematic.libtuf.data.CommonDataType.HashMethod.HashMethod
import com.advancedtelematic.libtuf.data.CommonDataType.SignatureMethod.SignatureMethod
import eu.timepit.refined.api.{Refined, Validate}
import org.genivi.sota.data.{CirceEnum, SlickEnum}

import scala.util.Try

// TODO: Move out
object CommonDataType {
  object HashMethod extends CirceEnum {
    type HashMethod = Value

    val SHA256 = Value("sha256")
  }

  case class Checksum(method: HashMethod, hash: Refined[String, ValidChecksum])

  case class ValidChecksum()

  implicit val validChecksumValidate: Validate.Plain[String, ValidChecksum] =
    ValidationUtils.validHexValidation(ValidChecksum(), length = 64)

  case class ValidKeyId()
  type KeyId = Refined[String, ValidKeyId]
  implicit val validKeyId: Validate.Plain[String, ValidKeyId] =
    ValidationUtils.validHexValidation(ValidKeyId(), length = 64)

  case class ValidSignature()
  case class
  Signature(hex: Refined[String, ValidSignature], method: SignatureMethod = SignatureMethod.RSASSA_PSS)
  implicit val validSignature: Validate.Plain[String, ValidSignature] =
    ValidationUtils.validHexValidation(ValidSignature(), length = 256)


  object KeyType extends CirceEnum with SlickEnum {
    type KeyType = Value

    val RSA = Value
  }

  object RoleType extends CirceEnum with SlickEnum {
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
}
