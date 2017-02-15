package com.advancedtelematic.ota_tuf.data

import java.security.PublicKey
import java.time.Instant
import cats.syntax.show.toShowOps
import com.advancedtelematic.ota_tuf.data.DataType.{KeyId, Signature, ValidSignature}
import com.advancedtelematic.ota_tuf.data.KeyType.KeyType
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.HashMethod.HashMethod
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.{SignedRole, ValidChecksum}
import com.advancedtelematic.ota_tuf.data.RoleType.RoleType
import com.advancedtelematic.ota_tuf.data.SignatureMethod.SignatureMethod
import eu.timepit.refined.api.{Refined, Validate}
import io.circe.{Encoder, Json}
import RoleType.show

object ClientDataType {
  case class ClientSignature(keyid: KeyId, method: SignatureMethod, sig: Refined[String, ValidSignature]) {
    def toSignature: Signature = Signature(sig, method)
  }

  implicit class SignatureToClientSignatureOps(value: Signature) {
    def toClient(keyId: KeyId): ClientSignature =
      ClientSignature(keyId, value.method, value.hex)
  }

  case class ClientKey(keytype: KeyType, keyval: PublicKey)

  case class SignedPayload[T : Encoder](signatures: Seq[ClientSignature], signed: T)

  case class RootRole(keys: Map[KeyId, ClientKey],
                      roles: Map[String, RoleKeys],
                      version: Int,
                      _type: String = "Root")

  case class RoleKeys(keyids: Seq[KeyId], threshold: Int)
}

object RepoClientDataType {
  type ClientHashes = Map[HashMethod, Refined[String, ValidChecksum]]

  case class ClientTargetItem(hashes: ClientHashes,
                              length: Long, custom: Json = Json.Null)

  case class ValidMetaPath()
  type MetaPath = Refined[String, ValidMetaPath]

  implicit val validMetaPath: Validate.Plain[String, ValidMetaPath] =
    Validate.fromPredicate(
      _.endsWith(".json"),
      str => s"$str is not a valid meta path, it needs to end in .json",
      ValidMetaPath())

  implicit class RoleTypeToMetaPathOp(value: RoleType) {
    def toMetaPath: MetaPath =
      RefinedUtils.refineTry[String, ValidMetaPath](value.show + ".json").get
  }

  case class MetaItem(hashes: ClientHashes, length: Long)

  implicit class SignedRoleMetaItemOps(signedRole: SignedRole) {
    def asMetaRole: (MetaPath, MetaItem) = {
      val hashes = Map(signedRole.checksum.method -> signedRole.checksum.hash)
      signedRole.roleType.toMetaPath -> MetaItem(hashes, signedRole.length)
    }
  }

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
