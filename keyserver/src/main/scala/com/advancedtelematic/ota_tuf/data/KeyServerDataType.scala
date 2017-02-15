package com.advancedtelematic.ota_tuf.data

import java.security.PublicKey
import java.util.UUID

import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RepoId}
import com.advancedtelematic.libtuf.data.TufDataType.KeyType.KeyType
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.UUIDKey.{UUIDKey, UUIDKeyObj}
import com.advancedtelematic.ota_tuf.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import org.genivi.sota.data.{CirceEnum, SlickEnum}

object KeyServerDataType {
  object KeyGenRequestStatus extends CirceEnum with SlickEnum {
    type KeyGenRequestStatus = Value

    val REQUESTED, GENERATED, ERROR = Value
  }

  case class KeyGenId(uuid: UUID) extends UUIDKey
  object KeyGenId extends UUIDKeyObj[KeyGenId]

  case class RoleId(uuid: UUID) extends UUIDKey
  object RoleId extends UUIDKeyObj[RoleId]

  case class KeyGenRequest(id: KeyGenId, repoId: RepoId,
                           status: KeyGenRequestStatus, roleType: RoleType,
                           keySize: Int = 1024, threshold: Int = 1)

  case class Key(id: KeyId, roleId: RoleId, keyType: KeyType, publicKey: PublicKey)

  case class Role(id: RoleId, repoId: RepoId, roleType: RoleType, threshold: Int = 1)
}


