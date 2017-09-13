package com.advancedtelematic.libtuf.data

import java.time.Instant

import com.advancedtelematic.libats.messaging_datatype.MessageCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, RoleType, TargetName, TargetVersion}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.circe._
import cats.syntax.either._

object ClientCodecs {
  import TufCodecs._
  import io.circe.generic.semiauto._
  import RefinedStringEncoding._
  import com.advancedtelematic.libats.codecs.AkkaCirce._

  implicit val roleTypeKeyEncoder: KeyEncoder[RoleType] = KeyEncoder.encodeKeyString.contramap[RoleType](_.toString.toLowerCase)
  implicit val roleTypeKeyDecoder: KeyDecoder[RoleType] = KeyDecoder.decodeKeyString.map[RoleType](s => RoleType.withName(s.toUpperCase))

  implicit val roleKeyEncoder: Encoder[RoleKeys] = deriveEncoder
  implicit val roleKeyDecoder: Decoder[RoleKeys] = deriveDecoder

  implicit val rootRoleEncoder: Encoder[RootRole] = deriveEncoder
  implicit val rootRoleDecoder: Decoder[RootRole] = deriveDecoder

  val legacyTargetCustomDecoder = Decoder.instance { cursor =>
    val now = Instant.now
    for {
      name <- cursor.downField("name").downField("value").as[TargetName]
      version <- cursor.downField("version").downField("value").as[TargetVersion]
      hardwareids <- cursor.downField("hardwareIds").as[Seq[HardwareIdentifier]]
      format <- cursor.downField("targetFormat").as[Option[TargetFormat]]
      createdAt <- cursor.downField("createdAt").as[Option[Instant]]
      updatedAt <- cursor.downField("updatedAt").as[Option[Instant]]
    } yield TargetCustom(name, version, hardwareids, format, createdAt.getOrElse(now), updatedAt.getOrElse(now))
  }

  val targetCustomDerivedDecoder = deriveDecoder[TargetCustom]

  implicit val targetCustomDecoder: Decoder[TargetCustom] = legacyTargetCustomDecoder or targetCustomDerivedDecoder
  implicit val targetCustomEncoder: Encoder[TargetCustom] = deriveEncoder

  implicit val targetsRoleEncoder: Encoder[TargetsRole] = deriveEncoder
  implicit val targetsRoleDecoder: Decoder[TargetsRole] = deriveDecoder

  implicit val targetNameEncoder: Encoder[TargetName] = anyValStringEncoder
  implicit val targetNameDecoder: Decoder[TargetName] = anyValStringDecoder

  implicit val targetVersionEncoder: Encoder[TargetVersion] = anyValStringEncoder
  implicit val targetVersionDecoder: Decoder[TargetVersion] = anyValStringDecoder

  implicit val clientTargetItemEncoder: Encoder[ClientTargetItem] = deriveEncoder
  implicit val clientTargetItemDecoder: Decoder[ClientTargetItem] = deriveDecoder

  implicit val metaItemEncoder: Encoder[MetaItem] = deriveEncoder
  implicit val metaItemDecoder: Decoder[MetaItem] = deriveDecoder

  implicit val snapshotRoleEncoder: Encoder[SnapshotRole] = deriveEncoder
  implicit val snapshotRoleDecoder: Decoder[SnapshotRole] = deriveDecoder

  implicit val timestampRoleEncoder: Encoder[TimestampRole] = deriveEncoder
  implicit val timestampRoleDecoder: Decoder[TimestampRole] = deriveDecoder
}
