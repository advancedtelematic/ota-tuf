package com.advancedtelematic.tuf.reposerver.data

import java.net.URI
import java.time.Instant

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libats.data.DataType.Checksum
import com.advancedtelematic.libtuf.data.ClientDataType.{MetaItem, MetaPath, _}
import com.advancedtelematic.libtuf.data.TufDataType.{JsonSignedPayload, RepoId, TargetFilename}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import io.circe.syntax._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf_server.crypto.Sha256Digest
import com.advancedtelematic.libtuf.data.ClientCodecs._

object RepositoryDataType {
  object StorageMethod extends Enumeration {
    type StorageMethod = Value
    val Managed, Unmanaged = Value
  }

  import StorageMethod._

  case class TargetItem(repoId: RepoId, filename: TargetFilename, uri: Option[Uri], checksum: Checksum, length: Long, custom: Option[TargetCustom] = None, storageMethod: StorageMethod = Managed)

  case class SignedRole(repoId: RepoId, roleType: RoleType, content: JsonSignedPayload, checksum: Checksum, length: Long, version: Int, expireAt: Instant)

//  case class SignedRoleNotDbLOL[T : TufRole](content: JsonSignedPayload, checksum: Checksum, length: Long, version: Int, expiresAt: Instant) {}

  // TODO:SM Surely there is a better way?
  // Maybe we could actually merge SignedRole and SignedRoleNotDBLOL ? make SignedRole[T] ?
//  implicit class SignedRoleAsLol(signedRole: SignedRole) {
//    def asLOL[T : TufRole]: SignedRoleNotDbLOL[T] =
//      SignedRoleNotDbLOL[T](signedRole.content, signedRole.checksum, signedRole.length, signedRole.version, signedRole.expireAt)
//  }

  implicit class SignedRoleMetaItemOps(signedRole: SignedRole) {
    def asMetaRole: (MetaPath, MetaItem) = {
      val hashes = Map(signedRole.checksum.method -> signedRole.checksum.hash)
      signedRole.roleType.toMetaPath -> MetaItem(hashes, signedRole.length, signedRole.version)
    }
  }

  // TODO:SM Duplicated
//  implicit class SignedRoleMetaItemOps2[T](signedRole: SignedRoleNotDbLOL[T])(implicit tufRole: TufRole[T]) {
//    def asMetaRole: (MetaPath, MetaItem) = {
//      val hashes = Map(signedRole.checksum.method -> signedRole.checksum.hash)
//      tufRole.toMetaPath -> MetaItem(hashes, signedRole.length, signedRole.version)
//    }
//  }


  implicit class JavaUriToAkkaUriConversion(value: URI) {
    def toUri: Uri = Uri(value.toString)
  }

  implicit class AkkaUriToJavaUriConversion(value: Uri) {
    def toURI: URI = URI.create(value.toString)
  }

  object SignedRole {
    def withChecksum(repoId: RepoId, roleType: RoleType, content: JsonSignedPayload, version: Int, expireAt: Instant): SignedRole = {
      val canonicalJson = content.asJson.canonical
      val checksum = Sha256Digest.digest(canonicalJson.getBytes)
      SignedRole(repoId, roleType, content, checksum, canonicalJson.length, version, expireAt: Instant)
    }
  }
}
