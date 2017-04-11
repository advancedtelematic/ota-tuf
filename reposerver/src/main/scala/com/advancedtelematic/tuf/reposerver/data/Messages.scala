package com.advancedtelematic.tuf.reposerver.data

import java.time.Instant

import com.advancedtelematic.libats.messaging.Messages.MessageLike
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._

object Messages {
  final case class PackageStorageUsage(namespace: String, timestamp: Instant, byteCount: Long)

  import com.advancedtelematic.libats.codecs.AkkaCirce.{dateTimeDecoder, dateTimeEncoder}

  implicit val packageStorageUsageEncoder: Encoder[PackageStorageUsage] = deriveEncoder
  implicit val packageStorageUsageDecoder: Decoder[PackageStorageUsage] = deriveDecoder

  implicit val packageStorageUsageMessageLike = MessageLike[PackageStorageUsage](_.namespace)
}
