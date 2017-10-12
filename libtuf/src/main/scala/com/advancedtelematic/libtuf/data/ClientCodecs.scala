package com.advancedtelematic.libtuf.data

import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.{RoleType, TargetFormat, TargetName, TargetVersion}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

object ClientCodecs {
  import TufCodecs._
  import io.circe.generic.semiauto._
  import com.advancedtelematic.libats.codecs.CirceCodecs._

  implicit val targetFormatEncoder: Encoder[TargetFormat] = Encoder.enumEncoder(TargetFormat)
  implicit val targetFormatDecoder: Decoder[TargetFormat] = Decoder.enumDecoder(TargetFormat)

  implicit val roleTypeKeyEncoder: KeyEncoder[RoleType] = KeyEncoder.encodeKeyString.contramap[RoleType](_.toString.toLowerCase)
  implicit val roleTypeKeyDecoder: KeyDecoder[RoleType] = KeyDecoder.decodeKeyString.map[RoleType](s => RoleType.withName(s.toUpperCase))

  implicit val roleKeyEncoder: Encoder[RoleKeys] = deriveEncoder
  implicit val roleKeyDecoder: Decoder[RoleKeys] = deriveDecoder

  implicit val rootRoleEncoder: Encoder[RootRole] = deriveEncoder
  implicit val rootRoleDecoder: Decoder[RootRole] = deriveDecoder

  implicit val targetCustomDecoder: Decoder[TargetCustom] = deriveDecoder
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
