package com.advancedtelematic.libtuf.data

import com.advancedtelematic.libtuf.data.ClientDataType._
import io.circe.{Decoder, Encoder}

object ClientCodecs {
  import TufCodecs._
  import io.circe.generic.semiauto._
  import RefinedStringEncoding._
  import com.advancedtelematic.libats.codecs.AkkaCirce._

  implicit val roleKeyEncoder: Encoder[RoleKeys] = deriveEncoder
  implicit val roleKeyDecoder: Decoder[RoleKeys] = deriveDecoder

  implicit val clientKeyEncoder: Encoder[ClientKey] = deriveEncoder
  implicit val clientKeyDecoder: Decoder[ClientKey] = deriveDecoder

  implicit val clientPrivateKeyEncoder: Encoder[ClientPrivateKey] = deriveEncoder
  implicit val clientPrivateKeyDecoder: Decoder[ClientPrivateKey] = deriveDecoder

  implicit val rootRoleEncoder: Encoder[RootRole] = deriveEncoder
  implicit val rootRoleDecoder: Decoder[RootRole] = deriveDecoder

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
