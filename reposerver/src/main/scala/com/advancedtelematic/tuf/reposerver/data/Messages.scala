package com.advancedtelematic.tuf.reposerver.data

import java.time.Instant

import com.advancedtelematic.libats.messaging.Messages.MessageLike
import com.advancedtelematic.libats.messaging_datatype.DataType.{TargetFilename, ValidTargetFilename}
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.Checksum

import cats.syntax.either._
import eu.timepit.refined

import io.circe.{Decoder, DecodingFailure, Encoder}
import io.circe.generic.semiauto._


object Messages {
  final case class PackageStorageUsage(namespace: String, timestamp: Instant, byteCount: Long)

  final case class TufTargetAdded(
    namespace: String,
    filename: TargetFilename,
    checksum: Checksum,
    length: Long,
    custom: Option[TargetCustom])

  import com.advancedtelematic.libats.codecs.AkkaCirce.{dateTimeDecoder, dateTimeEncoder}

  implicit val packageStorageUsageEncoder: Encoder[PackageStorageUsage] = deriveEncoder
  implicit val packageStorageUsageDecoder: Decoder[PackageStorageUsage] = deriveDecoder

  implicit val packageStorageUsageMessageLike = MessageLike[PackageStorageUsage](_.namespace)

  import com.advancedtelematic.libtuf.data.ClientCodecs._
  import com.advancedtelematic.libtuf.data.TufCodecs._

  implicit val targetFilenameEncoder: Encoder[TargetFilename] = Encoder.encodeString.contramap[TargetFilename](_.value)
  implicit val targetFilenameDecoder: Decoder[TargetFilename] = Decoder.instance { c =>

    import com.advancedtelematic.libats.messaging_datatype.DataType._
    c.focus.flatMap(_.asString) match {
      case None => Either.left(DecodingFailure("TargetFilename", c.history))
      case Some(str) =>
        refined.refineV[ValidTargetFilename](str) match {
          case Left(err) => Either.left(DecodingFailure("TargetFilename", c.history))
          case Right(v) => Either.right(v)
        }
    }
  }

  implicit val tufTargetAddedMessageLike = MessageLike[TufTargetAdded](_.namespace)
}
