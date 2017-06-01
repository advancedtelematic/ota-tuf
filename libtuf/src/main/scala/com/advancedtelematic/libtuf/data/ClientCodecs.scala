package com.advancedtelematic.libtuf.data

import com.advancedtelematic.libats.messaging_datatype.MessageCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.{RoleType, TargetName, TargetVersion}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

object ClientCodecs {
  import TufCodecs._
  import io.circe.generic.semiauto._
  import RefinedStringEncoding._
  import com.advancedtelematic.libats.codecs.AkkaCirce._

  implicit val roleTypeKeyEncoder: KeyEncoder[RoleType] = KeyEncoder.encodeKeyString.contramap[RoleType](_.toString.toLowerCase)
  implicit val roleTypeKeyDecoder: KeyDecoder[RoleType] = KeyDecoder.decodeKeyString.map[RoleType](s => RoleType.withName(s.toUpperCase))

  implicit val roleKeyEncoder: Encoder[RoleKeys] = deriveEncoder
  implicit val roleKeyDecoder: Decoder[RoleKeys] = deriveDecoder

  implicit val clientKeyEncoder: Encoder[ClientKey] = deriveEncoder
  implicit val clientKeyDecoder: Decoder[ClientKey] = deriveDecoder

  implicit val clientPrivateKeyEncoder: Encoder[ClientPrivateKey] = deriveEncoder
  implicit val clientPrivateKeyDecoder: Decoder[ClientPrivateKey] = deriveDecoder

  implicit val rootRoleEncoder: Encoder[RootRole] = deriveEncoder
  implicit val rootRoleDecoder: Decoder[RootRole] = deriveDecoder

  implicit val targetCustomEncoder: Encoder[TargetCustom] = deriveEncoder
  implicit val targetCustomDecoder: Decoder[TargetCustom] = deriveDecoder

  implicit val targetsRoleEncoder: Encoder[TargetsRole] = deriveEncoder
  implicit val targetsRoleDecoder: Decoder[TargetsRole] = deriveDecoder

  implicit val targetNameEncoder: Encoder[TargetName] = deriveEncoder
  implicit val targetNameDecoder: Decoder[TargetName] = deriveDecoder

  implicit val targetVersionEncoder: Encoder[TargetVersion] = deriveEncoder
  implicit val targetVersionDecoder: Decoder[TargetVersion] = deriveDecoder

  implicit val clientTargetItemEncoder: Encoder[ClientTargetItem] = deriveEncoder
  implicit val clientTargetItemDecoder: Decoder[ClientTargetItem] = deriveDecoder

  implicit val metaItemEncoder: Encoder[MetaItem] = deriveEncoder
  implicit val metaItemDecoder: Decoder[MetaItem] = deriveDecoder

  implicit val snapshotRoleEncoder: Encoder[SnapshotRole] = deriveEncoder
  implicit val snapshotRoleDecoder: Decoder[SnapshotRole] = deriveDecoder

  implicit val timestampRoleEncoder: Encoder[TimestampRole] = deriveEncoder
  implicit val timestampRoleDecoder: Decoder[TimestampRole] = deriveDecoder
}
