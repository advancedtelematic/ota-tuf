package com.advancedtelematic.libtuf.data

import java.security.PublicKey

import akka.http.scaladsl.model.Uri
import cats.syntax.show.toShowOps
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.crypt.RsaKeyPair._
import com.advancedtelematic.libtuf.data.TufDataType.Checksum
import io.circe.Json
import slick.driver.MySQLDriver.api._
import cats.syntax.either._

import scala.reflect.ClassTag

object SlickPublicKeyMapper {
  implicit val publicKeyMapper = MappedColumnType.base[PublicKey, String](
    {publicKey => publicKey.show},
    {str  => RsaKeyPair.parsePublic(str).get}
  )
}

object SlickUriMapper {
  implicit val uriMapper = MappedColumnType.base[Uri, String](
    _.toString,
    Uri.apply
  )
}

object SlickCirceMapper {
  import io.circe.parser.decode
  import io.circe.syntax._
  import io.circe.{Decoder, Encoder}

  /*
   It's easy to misuse this if it's public, if there is an encoder in scope
   for an object, it will encode it as json instead of it's default encoding
   so we define public mappers for specific types
   */
  def circeMapper[T : Encoder : Decoder : ClassTag] = MappedColumnType.base[T, String](
    _.asJson.noSpaces,
    str => decode(str).valueOr(throw _)
  )

  import TufCodecs.{checkSumDecoder, checkSumEncoder}

  implicit val checksumMapper = circeMapper[Checksum]

  implicit val jsonMapper = circeMapper[Json]
}
