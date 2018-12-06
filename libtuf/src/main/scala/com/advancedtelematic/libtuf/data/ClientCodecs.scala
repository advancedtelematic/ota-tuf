package com.advancedtelematic.libtuf.data

import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.{RoleType, TargetFormat, TargetName, TargetVersion}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import io.circe._
import ClientDataType.TufRole._
import cats.syntax.either._

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

  implicit val targetCustomDecoder: Decoder[TargetCustom] = deriveDecoder
  implicit val targetCustomEncoder: Encoder[TargetCustom] = deriveEncoder

  implicit val targetNameEncoder: Encoder[TargetName] = anyValStringEncoder
  implicit val targetNameDecoder: Decoder[TargetName] = anyValStringDecoder

  implicit val targetVersionEncoder: Encoder[TargetVersion] = anyValStringEncoder
  implicit val targetVersionDecoder: Decoder[TargetVersion] = anyValStringDecoder

  implicit val clientTargetItemEncoder: Encoder[ClientTargetItem] = deriveEncoder
  implicit val clientTargetItemDecoder: Decoder[ClientTargetItem] = deriveDecoder

  implicit val metaItemEncoder: Encoder[MetaItem] = deriveEncoder
  implicit val metaItemDecoder: Decoder[MetaItem] = deriveDecoder

  implicit val rootRoleEncoder: Encoder[RootRole] = deriveEncoder[RootRole].encodeRoleType
  implicit val rootRoleDecoder: Decoder[RootRole] = deriveDecoder[RootRole].validateRoleType

  implicit val delegatedRoleNameEncoder: Encoder[DelegatedRoleName] = ValidatedString.validatedStringEncoder
  implicit val delegatedRoleNameDecoder: Decoder[DelegatedRoleName] = ValidatedString.validatedStringDecoder

  implicit val delegatedPathPatternEncoder: Encoder[DelegatedPathPattern] = ValidatedString.validatedStringEncoder
  implicit val delegatedPathPatternDecoder: Decoder[DelegatedPathPattern] = ValidatedString.validatedStringDecoder

  implicit val delegatedRoleEncoder: Encoder[Delegation] = deriveEncoder
  implicit val delegatedRoleDecoder: Decoder[Delegation] = deriveDecoder

  implicit val delegationsEncoder: Encoder[Delegations] = deriveEncoder
  implicit val delegationsDecoder: Decoder[Delegations] = deriveDecoder[Delegations]

  implicit val targetsRoleEncoder: Encoder[TargetsRole] = deriveEncoder[TargetsRole].encodeRoleType.mapJson(_.dropNullValues)
  implicit val targetsRoleDecoder: Decoder[TargetsRole] = deriveDecoder[TargetsRole].validateRoleType

  implicit val snapshotRoleEncoder: Encoder[SnapshotRole] = deriveEncoder[SnapshotRole].encodeRoleType
  implicit val snapshotRoleDecoder: Decoder[SnapshotRole] = deriveDecoder[SnapshotRole].validateRoleType

  implicit val timestampRoleEncoder: Encoder[TimestampRole] = deriveEncoder[TimestampRole].encodeRoleType
  implicit val timestampRoleDecoder: Decoder[TimestampRole] = deriveDecoder[TimestampRole].validateRoleType

  // TODO: Remove after https://github.com/circe/circe/pull/983/files is merged
  implicit private class JsonDropNullValues(value: Json) {
    def dropNullValues: Json = value.mapObject(_.filter { case (_, v) => !v.isNull })
  }

  implicit private class EncodeRoleTypeOp[T](encoder: Encoder[T])(implicit tr: TufRole[T]) {
    def encodeRoleType: Encoder[T] = encoder.mapJson(_.deepMerge(Json.obj("_type" -> Json.fromString(tr.typeStr))))
  }

  implicit private class ValidateRoleOp[T](decoder: Decoder[T])(implicit tr: TufRole[T]) {
    def validateRoleType: Decoder[T] = decoder.validate({ c =>
      val _type = c.downField("_type").as[String].valueOr(throw _).toLowerCase.capitalize
      _type == tr.typeStr
    },
      s"Invalid type for role: ${tr.roleType}"
    )
  }
}
