package com.advancedtelematic.tuf.keyserver.data

import java.security.PublicKey
import java.util.UUID

import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RepoId}
import com.advancedtelematic.libtuf.data.TufDataType.KeyType.KeyType
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.libats.codecs.CirceEnum
import com.advancedtelematic.libats.data.UUIDKey.{UUIDKey, UUIDKeyObj}
import com.advancedtelematic.libats.slick.codecs.SlickEnum

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
                           keySize: Int, threshold: Int = 1)

  case class Key(id: KeyId, roleId: RoleId, keyType: KeyType, publicKey: PublicKey)

  case class Role(id: RoleId, repoId: RepoId, roleType: RoleType, threshold: Int = 1)
}


