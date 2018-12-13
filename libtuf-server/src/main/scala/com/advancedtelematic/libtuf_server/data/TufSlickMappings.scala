package com.advancedtelematic.libtuf_server.data

import java.security.PublicKey

import com.advancedtelematic.libats.data.DataType.Checksum
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.crypt.TufCrypto.KeyOps
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.{EcPrime256KeyType, Ed25519KeyType, Ed25519TufKey, Ed25519TufPrivateKey, JsonSignedPayload, KeyType, RoleType, RsaKeyType, TufKey, TufPrivateKey}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libats.slick.db.{SlickCirceMapper, SlickEncryptedColumn}
import io.circe.Json
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libats.codecs.CirceAts._
import com.advancedtelematic.libats.slick.codecs.SlickEnumMapper
import net.i2p.crypto.eddsa.EdDSAPublicKey

import scala.util.{Failure, Success}

object TufSlickMappings {

  // TODO: Use getEncoded | base64 instead of pem. Add migration. Probably use TufKey if possible
  // TODO:SM Fix this mapping

  implicit val publicKeyMapper = MappedColumnType.base[PublicKey, String](
    {
      case edk: EdDSAPublicKey =>
        Ed25519KeyType.crypto.encode(Ed25519TufKey(edk)).noSpaces
      case publicKey => publicKey.toPem
    },
    {str =>
      io.circe.jawn.parse(str).flatMap(_.as[String]).toTry.flatMap(Ed25519KeyType.crypto.parsePublic) match {
        case Success(oi) => oi.keyval
        case Failure(ex) =>
          TufCrypto.parsePublicPem(str).get
      }
    }
  )

  implicit val keyTypeMapper = MappedColumnType.base[KeyType, String](
    {
      case RsaKeyType ⇒ "RSA"
      case Ed25519KeyType ⇒ "ED25519"
      case EcPrime256KeyType ⇒ "ECPRIME256V1"
    },
    {
      case "RSA" ⇒ RsaKeyType
      case "ED25519" ⇒ Ed25519KeyType
      case "ECPRIME256V1" ⇒ EcPrime256KeyType
    }
  )

  implicit val roleTypeMapper = SlickEnumMapper.enumMapper(RoleType)

  implicit val checksumMapper = SlickCirceMapper.circeMapper[Checksum]

  implicit val targetCustomMapper = SlickCirceMapper.circeMapper[TargetCustom]

  implicit val jsonSignedPayloadMapper = SlickCirceMapper.circeMapper[JsonSignedPayload]

  implicit val tufKeyMapper = SlickCirceMapper.circeMapper[TufKey]

  implicit val encryptedTufPrivateKeyMapper = SlickEncryptedColumn.encryptedColumnJsonMapper[TufPrivateKey]
}
