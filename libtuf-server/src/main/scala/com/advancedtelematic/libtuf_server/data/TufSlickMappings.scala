package com.advancedtelematic.libtuf_server.data

import java.security.PublicKey

import com.advancedtelematic.libats.codecs.CirceEnum
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.crypt.TufCrypto.KeyOps
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, EdKeyType, KeyType, RoleType, RsaKeyType, SignedPayload}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libats.slick.db.SlickCirceMapper
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import io.circe.{Decoder, Encoder, Json}
import slick.jdbc.MySQLProfile.api._

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

  private implicit val roleTypeEncoder = Encoder.enumEncoder(RoleType)

  private implicit val roleTypeDecoder = Decoder.enumDecoder(RoleType)

  implicit val roleTypeMapper = SlickCirceMapper.circeMapper[RoleType]

  implicit val checksumMapper = SlickCirceMapper.circeMapper[Checksum]

  implicit val targetCustomMapper = SlickCirceMapper.circeMapper[TargetCustom]

  implicit val jsonSignedPayloadMapper = SlickCirceMapper.circeMapper[SignedPayload[Json]]
}
