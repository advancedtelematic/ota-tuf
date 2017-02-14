package com.advancedtelematic.ota_tuf.data

import java.security.PublicKey
import java.util.UUID

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.PathMatchers
import cats.Show
import com.advancedtelematic.ota_tuf.crypt.Sha256Digest
import com.advancedtelematic.ota_tuf.data.DataType.RepoId
import com.advancedtelematic.ota_tuf.data.UUIDKey.{UUIDKey, UUIDKeyObj}
import com.advancedtelematic.ota_tuf.data.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.data.KeyType.KeyType
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.HashMethod.HashMethod
import com.advancedtelematic.ota_tuf.data.RoleType.RoleType
import com.advancedtelematic.ota_tuf.data.SignatureMethod.SignatureMethod
import eu.timepit.refined.api.{Refined, Validate}
import io.circe.Json
import org.genivi.sota.data.{CirceEnum, SlickEnum}
import com.advancedtelematic.ota_tuf.http.CanonicalJson._
import io.circe.syntax._

import scala.util.Try

object KeyGenRequestStatus extends CirceEnum with SlickEnum {
  type KeyGenRequestStatus = Value

  val REQUESTED, GENERATED, ERROR = Value
}

object KeyType extends CirceEnum with SlickEnum {
  type KeyType = Value

  val RSA = Value
}

object SignatureMethod extends CirceEnum {
  type SignatureMethod = Value

  val RSASSA_PSS = Value("rsassa-pss")
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

object DataType {
  case class KeyGenId(uuid: UUID) extends UUIDKey
  object KeyGenId extends UUIDKeyObj[KeyGenId]

  case class RoleId(uuid: UUID) extends UUIDKey
  object RoleId extends UUIDKeyObj[RoleId]

  case class RepoId(uuid: UUID) extends UUIDKey
  object RepoId extends UUIDKeyObj[RepoId]

  case class ValidKeyId()
  type KeyId = Refined[String, ValidKeyId]
  implicit val validKeyId: Validate.Plain[String, ValidKeyId] =
    ValidationUtils.validHexValidation(ValidKeyId(), length = 64)

  case class ValidSignature()
  case class
  Signature(hex: Refined[String, ValidSignature], method: SignatureMethod = SignatureMethod.RSASSA_PSS)
  implicit val validSignature: Validate.Plain[String, ValidSignature] =
    ValidationUtils.validHexValidation(ValidSignature(), length = 256)

  case class KeyGenRequest(id: KeyGenId, repoId: RepoId,
                           status: KeyGenRequestStatus, roleType: RoleType,
                           keySize: Int = 1024, threshold: Int = 1)

  case class Key(id: KeyId, roleId: RoleId, keyType: KeyType, publicKey: PublicKey)

  case class Role(id: RoleId, repoId: RepoId, roleType: RoleType, threshold: Int = 1)
}

object RepositoryDataType {

  object HashMethod extends CirceEnum {
    type HashMethod = Value

    val SHA256 = Value("sha256")
  }

  case class Checksum(method: HashMethod, hash: Refined[String, ValidChecksum])

  case class ValidChecksum()

  implicit val validChecksumValidate: Validate.Plain[String, ValidChecksum] =
    ValidationUtils.validHexValidation(ValidChecksum(), length = 64)

  case class TargetItem(repoId: RepoId, filename: String, uri: Uri, checksum: Checksum, length: Long)

  case class SignedRole(repoId: RepoId, roleType: RoleType, content: Json, checksum: Checksum, length: Long, version: Int)

  object SignedRole {
    def withChecksum(repoId: RepoId, roleType: RoleType, content: Json, version: Int): SignedRole = {
      val canonicalJson = content.canonical
      val checksum = Sha256Digest.digest(canonicalJson.getBytes)
      SignedRole(repoId, roleType, content, checksum, canonicalJson.length, version)
    }
  }
}


protected[data] object ValidationUtils {
  def validHex(length: Long, str: String): Boolean = {
    str.length == length && str.forall(h => ('0' to '9').contains(h) || ('a' to 'f').contains(h))
  }

  def validHexValidation[T](v: T, length: Int): Validate.Plain[String, T] =
    Validate.fromPredicate(
      hash => validHex(length, hash),
      hash => s"$hash is not a $length hex string",
      v
    )
}
