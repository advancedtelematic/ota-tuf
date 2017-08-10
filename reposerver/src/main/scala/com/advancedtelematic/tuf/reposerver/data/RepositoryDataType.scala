package com.advancedtelematic.tuf.reposerver.data

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libtuf.crypt.Sha256Digest
import com.advancedtelematic.libtuf.data.ClientDataType.{MetaItem, MetaPath, _}
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, RepoId, SignedPayload}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import io.circe.{Encoder, Json}
import io.circe.syntax._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._

object RepositoryDataType {
  case class TargetItem(repoId: RepoId, filename: TargetFilename, uri: Uri, checksum: Checksum, length: Long, custom: Option[TargetCustom] = None)

  case class SignedRole(repoId: RepoId, roleType: RoleType, content: SignedPayload[Json], checksum: Checksum, length: Long, version: Int)

  implicit class SignedRoleMetaItemOps(signedRole: SignedRole) {
    def asMetaRole: (MetaPath, MetaItem) = {
      val hashes = Map(signedRole.checksum.method -> signedRole.checksum.hash)
      signedRole.roleType.toMetaPath -> MetaItem(hashes, signedRole.length)
    }
  }

  object SignedRole {
    def withChecksum[T : Encoder](repoId: RepoId, roleType: RoleType, content: SignedPayload[T], version: Int): SignedRole = {
      val canonicalJson = content.asJson.canonical
      val checksum = Sha256Digest.digest(canonicalJson.getBytes)
      SignedRole(repoId, roleType, SignedPayload(content.signatures, content.signed.asJson), checksum, canonicalJson.length, version)
    }
  }
}
