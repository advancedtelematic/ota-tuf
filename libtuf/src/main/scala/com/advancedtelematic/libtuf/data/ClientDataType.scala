package com.advancedtelematic.libtuf.data

import java.security.{PrivateKey, PublicKey}
import java.time.Instant

import cats.syntax.show._
import com.advancedtelematic.libtuf.data.TufDataType.HashMethod.HashMethod
import com.advancedtelematic.libtuf.data.TufDataType.KeyType.KeyType
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.{RoleType, show}
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, ValidChecksum}
import eu.timepit.refined.api.{Refined, Validate}
import io.circe.Json
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry

object ClientDataType {
  type ClientHashes = Map[HashMethod, Refined[String, ValidChecksum]]

  case class ClientTargetItem(hashes: ClientHashes,
                              length: Long, custom: Json = Json.Null)


  case class ClientKey(keytype: KeyType, keyval: PublicKey)

  case class ClientPrivateKey(keytype: KeyType, keyval: PrivateKey)

  case class RootRole(keys: Map[KeyId, ClientKey],
                      roles: Map[String, RoleKeys],
                      version: Int,
                      expires: Instant,
                      consistent_snapshot: Boolean = false,
                      _type: String = "Root")

  case class RoleKeys(keyids: Seq[KeyId], threshold: Int)

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
  }

  case class MetaItem(hashes: ClientHashes, length: Long)

  trait VersionedRole {
    val version: Int
  }

  case class TargetsRole(expires: Instant,
                         targets: Map[String, ClientTargetItem],
                         version: Int,
                         _type: String = "Targets") extends VersionedRole

  case class SnapshotRole(meta: Map[MetaPath, MetaItem],
                          expires: Instant,
                          version: Int,
                          _type: String = "Snapshot") extends VersionedRole

  case class TimestampRole(
                            meta: Map[MetaPath, MetaItem],
                            expires: Instant,
                            version: Int,
                            _type: String = "Timestamp") extends VersionedRole
}
