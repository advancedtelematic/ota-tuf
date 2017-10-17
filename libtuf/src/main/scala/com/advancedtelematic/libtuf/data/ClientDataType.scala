package com.advancedtelematic.libtuf.data

import java.net.URI
import java.time.Instant

import cats.syntax.show._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, KeyId, RoleType, TargetFilename, TargetName, TargetVersion, TufKey}
import eu.timepit.refined.api.{Refined, Validate}
import io.circe.{Decoder, Json}
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import cats.syntax.either._
import com.advancedtelematic.libats.data.DataType.HashMethod.HashMethod
import com.advancedtelematic.libats.data.DataType.ValidChecksum

object ClientDataType {
  type ClientHashes = Map[HashMethod, Refined[String, ValidChecksum]]

  case class TargetCustom(name: TargetName, version: TargetVersion, hardwareIds: Seq[HardwareIdentifier],
                          targetFormat: Option[TargetFormat],
                          uri: Option[URI] = None,
                          createdAt: Instant = Instant.now,
                          updatedAt: Instant = Instant.now
                         )

  case class ClientTargetItem(hashes: ClientHashes,
                              length: Long, custom: Option[Json]) {
    def customParsed[T](implicit decoder: Decoder[T]): Option[T] =
      custom.flatMap(c => decoder.decodeJson(c).toOption)
  }

  case class RoleKeys(keyids: Seq[KeyId], threshold: Int)

  case class ETag(value: String) extends AnyVal

  case class ValidMetaPath()
  type MetaPath = Refined[String, ValidMetaPath]

  implicit val validMetaPath: Validate.Plain[String, ValidMetaPath] =
    Validate.fromPredicate(
      _.endsWith(".json"),
      str => s"$str is not a valid meta path, it needs to end in .json",
      ValidMetaPath())

  implicit class RoleTypeToMetaPathOp(value: RoleType) {
    def toMetaPath: MetaPath =
      (value.show + ".json").refineTry[ValidMetaPath].get

    def toETagPath: String =
      toMetaPath.value + ".etag"
  }

  case class MetaItem(hashes: ClientHashes, length: Long, version: Int)

  sealed trait VersionedRole {
    val version: Int
    val expires: Instant

    def roleType: RoleType = this match {
      case _: TargetsRole => RoleType.TARGETS
      case _: SnapshotRole => RoleType.SNAPSHOT
      case _: TimestampRole => RoleType.TIMESTAMP
      case _: RootRole => RoleType.ROOT
    }
  }

  case class RootRole(keys: Map[KeyId, TufKey],
                      roles: Map[RoleType, RoleKeys],
                      version: Int,
                      expires: Instant,
                      consistent_snapshot: Boolean = false,
                      _type: String = "Root") extends VersionedRole

  case class TargetsRole(expires: Instant,
                         targets: Map[TargetFilename, ClientTargetItem],
                         version: Int,
                         _type: String = "Targets") extends VersionedRole

  case class SnapshotRole(meta: Map[MetaPath, MetaItem],
                          expires: Instant,
                          version: Int,
                          _type: String = "Snapshot") extends VersionedRole

  case class TimestampRole(meta: Map[MetaPath, MetaItem],
                           expires: Instant,
                           version: Int,
                           _type: String = "Timestamp") extends VersionedRole
}
