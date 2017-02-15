package com.advancedtelematic.ota_tuf.data

import java.security.PublicKey
import java.util.UUID

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.PathMatchers
import cats.Show
import com.advancedtelematic.libtuf.crypt.Sha256Digest
import com.advancedtelematic.libtuf.data.CommonDataType.KeyType.KeyType
import com.advancedtelematic.libtuf.data.CommonDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.CommonDataType.{Checksum, KeyId}
import com.advancedtelematic.libtuf.data.RepoClientDataType.{MetaItem, MetaPath}
import com.advancedtelematic.libtuf.data.UUIDKey.{UUIDKey, UUIDKeyObj}
import com.advancedtelematic.libtuf.data.ValidationUtils
import com.advancedtelematic.ota_tuf.data.DataType.RepoId
import com.advancedtelematic.ota_tuf.data.DataType.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.http.CanonicalJson._
import eu.timepit.refined.api.{Refined, Validate}
import io.circe.Json
import org.genivi.sota.data.{CirceEnum, SlickEnum}
import com.advancedtelematic.libtuf.data.RepoClientDataType._

object DataType {
  object KeyGenRequestStatus extends CirceEnum with SlickEnum {
    type KeyGenRequestStatus = Value

    val REQUESTED, GENERATED, ERROR = Value
  }

  case class KeyGenId(uuid: UUID) extends UUIDKey
  object KeyGenId extends UUIDKeyObj[KeyGenId]

  case class RoleId(uuid: UUID) extends UUIDKey
  object RoleId extends UUIDKeyObj[RoleId]

  case class RepoId(uuid: UUID) extends UUIDKey
  object RepoId extends UUIDKeyObj[RepoId]

  case class KeyGenRequest(id: KeyGenId, repoId: RepoId,
                           status: KeyGenRequestStatus, roleType: RoleType,
                           keySize: Int = 1024, threshold: Int = 1)

  case class Key(id: KeyId, roleId: RoleId, keyType: KeyType, publicKey: PublicKey)

  case class Role(id: RoleId, repoId: RepoId, roleType: RoleType, threshold: Int = 1)
}


