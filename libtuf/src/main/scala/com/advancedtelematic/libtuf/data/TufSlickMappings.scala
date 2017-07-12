package com.advancedtelematic.libtuf.data

import java.security.PublicKey

import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.crypt.TufCrypto.KeyOps
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, EdKeyType, KeyType, RsaKeyType}
import ClientCodecs._
import TufCodecs._
import com.advancedtelematic.libats.slick.db.SlickCirceMapper

object TufSlickMappings {

  implicit val publicKeyMapper = MappedColumnType.base[PublicKey, String](
    {publicKey => publicKey.toPem},
    {str => TufCrypto.parsePublicPem(str).get}
  )

  implicit val keyTypeMapper = MappedColumnType.base[KeyType, String](
    {
      case RsaKeyType ⇒ "RSA"
      case EdKeyType ⇒ "ED25519"
    },
    {
      case "RSA" ⇒ RsaKeyType
      case "ED25519" ⇒ EdKeyType
    }
  )

  implicit val checksumMapper = SlickCirceMapper.circeMapper[Checksum]

  implicit val targetCustomMapper = SlickCirceMapper.circeMapper[TargetCustom]
}
