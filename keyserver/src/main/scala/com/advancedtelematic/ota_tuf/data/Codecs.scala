package com.advancedtelematic.ota_tuf.data

import java.security.PublicKey

import com.advancedtelematic.libtuf.crypt.RsaKeyPair._
import cats.syntax.show.toShowOps
import akka.http.scaladsl.model.Uri
import cats.data.Xor
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.HashMethod.HashMethod
import com.advancedtelematic.libtuf.data.TufDataType._
import RepoClientDataType._
import com.advancedtelematic.libtuf.data.TufCodecs._
import io.circe._

import scala.util.Try

object Codecs {
  import com.advancedtelematic.libtuf.data.RefinedStringEncoding._
  import io.circe.generic.semiauto._
  import org.genivi.sota.marshalling.CirceInstances.{dateTimeDecoder, dateTimeEncoder, refinedDecoder, refinedEncoder}


  implicit val targetsRoleEncoder: Encoder[TargetsRole] = deriveEncoder
  implicit val targetsRoleDecoder: Decoder[TargetsRole] = deriveDecoder

  implicit val clientTargetItemEncoder: Encoder[ClientTargetItem] = deriveEncoder
  implicit val clientTargetItemDecoder: Decoder[ClientTargetItem] = deriveDecoder

  implicit val metaItemEncoder: Encoder[MetaItem] = deriveEncoder
  implicit val metaItemDecoder: Decoder[MetaItem] = deriveDecoder

  implicit val snapshotRoleEncoder: Encoder[SnapshotRole] = deriveEncoder
  implicit val snapshotRoleDecoder: Decoder[SnapshotRole] = deriveDecoder

  implicit val timestampRoleEncoder: Encoder[TimestampRole] = deriveEncoder
  implicit val timestampRoleDecoder: Decoder[TimestampRole] = deriveDecoder
}


