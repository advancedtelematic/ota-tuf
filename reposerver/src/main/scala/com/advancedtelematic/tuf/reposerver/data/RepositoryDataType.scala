package com.advancedtelematic.tuf.reposerver.data

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libtuf.crypt.Sha256Digest
import com.advancedtelematic.libtuf.data.ClientDataType.{MetaItem, MetaPath, _}
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, RepoId}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import io.circe.Json
import com.advancedtelematic.libtuf.crypt.CanonicalJson._

object RepositoryDataType {
  case class TargetItem(repoId: RepoId, filename: String, uri: Uri, checksum: Checksum, length: Long)

  case class SignedRole(repoId: RepoId, roleType: RoleType, content: Json, checksum: Checksum, length: Long, version: Int)

  implicit class SignedRoleMetaItemOps(signedRole: SignedRole) {
    def asMetaRole: (MetaPath, MetaItem) = {
      val hashes = Map(signedRole.checksum.method -> signedRole.checksum.hash)
      signedRole.roleType.toMetaPath -> MetaItem(hashes, signedRole.length)
    }
  }

  object SignedRole {
    def withChecksum(repoId: RepoId, roleType: RoleType, content: Json, version: Int): SignedRole = {
      val canonicalJson = content.canonical
      val checksum = Sha256Digest.digest(canonicalJson.getBytes)
      SignedRole(repoId, roleType, content, checksum, canonicalJson.length, version)
    }
  }
}
