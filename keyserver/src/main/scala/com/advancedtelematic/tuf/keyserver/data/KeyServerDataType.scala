package com.advancedtelematic.tuf.keyserver.data

import java.security.PublicKey
import java.util.UUID

import com.advancedtelematic.libats.data.UUIDKey.{UUIDKey, UUIDKeyObj}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, KeyType, RepoId, TufKey}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus

object KeyServerDataType {
  object KeyGenRequestStatus extends Enumeration {
    type KeyGenRequestStatus = Value

    val REQUESTED, GENERATED, ERROR = Value
  }

  case class KeyGenId(uuid: UUID) extends UUIDKey
  object KeyGenId extends UUIDKeyObj[KeyGenId]

  case class KeyGenRequest(id: KeyGenId, repoId: RepoId,
                           status: KeyGenRequestStatus, roleType: RoleType,
                           keySize: Int,
                           keyType: KeyType,
                           threshold: Int = 1,
                           description: String = "") {
    require(keyType.crypto.validKeySize(keySize), s"Invalid keysize ($keySize) for $keyType")
  }

  case class Key(id: KeyId, repoId: RepoId, roleType: RoleType, keyType: KeyType, publicKey: PublicKey) {
    def toTufKey: TufKey = keyType.crypto.convertPublic(publicKey)
  }
}
