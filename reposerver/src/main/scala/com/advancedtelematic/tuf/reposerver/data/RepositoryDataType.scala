package com.advancedtelematic.tuf.reposerver.data

import java.net.URI
import java.time.Instant

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libats.data.DataType.Checksum
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.data.ClientDataType.{MetaItem, MetaPath, _}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{JsonSignedPayload, RepoId, TargetFilename}
import com.advancedtelematic.libtuf_server.crypto.Sha256Digest
import io.circe.Decoder
import io.circe.syntax._

object RepositoryDataType {
  object StorageMethod extends Enumeration {
    type StorageMethod = Value
    val Managed, Unmanaged = Value
  }

  import StorageMethod._

  case class TargetItem(repoId: RepoId, filename: TargetFilename, uri: Option[Uri], checksum: Checksum, length: Long, custom: Option[TargetCustom] = None, storageMethod: StorageMethod = Managed)

  case class SignedRole[T : TufRole](repoId: RepoId, content: JsonSignedPayload, checksum: Checksum, length: Long, version: Int, expiresAt: Instant) {
    def role(implicit dec: Decoder[T]): T =
      content.signed.as[T] match {
        case Left(err) =>
          throw new IllegalArgumentException(s"Could not decode a role saved in database as ${implicitly[TufRole[T]]} but not parseable as such a type: $err")
        case Right(p) => p
      }

    def asMetaRole: (MetaPath, MetaItem) = {
      val hashes = Map(checksum.method -> checksum.hash)
      tufRole.metaPath -> MetaItem(hashes, length, version)
    }

    def tufRole: TufRole[T] = implicitly[TufRole[T]]
  }

  object SignedRole {
    def withChecksum[T : TufRole](repoId: RepoId, content: JsonSignedPayload, version: Int, expireAt: Instant): SignedRole[T] = {
      val canonicalJson = content.asJson.canonical
      val checksum = Sha256Digest.digest(canonicalJson.getBytes)
      SignedRole[T](repoId, content, checksum, canonicalJson.length, version, expireAt)
    }
  }

  implicit class JavaUriToAkkaUriConversion(value: URI) {
    def toUri: Uri = Uri(value.toString)
  }

  implicit class AkkaUriToJavaUriConversion(value: Uri) {
    def toURI: URI = URI.create(value.toString)
  }
}
