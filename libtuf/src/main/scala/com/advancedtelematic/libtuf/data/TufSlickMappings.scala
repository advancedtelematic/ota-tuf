package com.advancedtelematic.libtuf.data

import java.security.PublicKey

import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import cats.syntax.show.toShowOps
import RsaKeyPair.keyShow
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.Checksum
import ClientCodecs._
import TufCodecs._
import com.advancedtelematic.libats.slick.db.SlickCirceMapper

object TufSlickMappings {
  implicit val publicKeyMapper = MappedColumnType.base[PublicKey, String](
    {publicKey => publicKey.show},
    {str  => RsaKeyPair.parsePublic(str).get}
  )

  implicit val checksumMapper = SlickCirceMapper.circeMapper[Checksum]

  implicit val targetCustomMapper = SlickCirceMapper.circeMapper[TargetCustom]
}
