package com.advancedtelematic.libtuf_server.data

import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.KeyType
import io.circe.{Decoder, Encoder}

object Requests {

  case class CreateRepositoryRequest(keyType: KeyType)

  implicit val encoder: Encoder[CreateRepositoryRequest] = io.circe.generic.semiauto.deriveEncoder
  implicit val decoder: Decoder[CreateRepositoryRequest] = io.circe.generic.semiauto.deriveDecoder

}
