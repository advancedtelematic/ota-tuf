package com.advancedtelematic.ota_tuf.db

import java.security.PublicKey

import akka.http.scaladsl.model.Uri
import cats.syntax.show.toShowOps
import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair
import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair._
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.Checksum
import io.circe.Json

import scala.reflect.ClassTag
import slick.driver.MySQLDriver.api._

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
  import io.circe.syntax._
  import io.circe.{Encoder, Decoder}
  import io.circe.parser.decode

  /*
   It's easy toi misuse this if it's public, if there is an encoder in scope
   for an object, it will encode it as json instead of it's default encoding
   so we define public mappers for specific types
   */
  private def circeMapper[T : Encoder : Decoder : ClassTag] = MappedColumnType.base[T, String](
    _.asJson.noSpaces,
    str => decode(str).valueOr(throw _)
  )

  import com.advancedtelematic.ota_tuf.data.Codecs.{checkSumDecoder, checkSumEncoder}
  implicit val checksumMapper = circeMapper[Checksum]

  implicit val jsonMapper = circeMapper[Json]
}