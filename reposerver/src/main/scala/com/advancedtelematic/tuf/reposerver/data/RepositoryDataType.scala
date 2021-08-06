package com.advancedtelematic.tuf.reposerver.data

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libats.data.DataType.{Checksum, Namespace}
import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, TargetFilename}

object RepositoryDataType {
  object StorageMethod {
    sealed trait StorageMethod
    object Managed extends StorageMethod
    object Unmanaged extends StorageMethod
    object CliManaged extends StorageMethod
  }

  import StorageMethod._

  case class TargetItem(repoId: RepoId, filename: TargetFilename, uri: Option[Uri], checksum: Checksum, length: Long, custom: Option[TargetCustom] = None, storageMethod: StorageMethod = Managed)

  case class RepoNamespace(repoId: RepoId, namespace: Namespace)

  object RepoNamespace {
    import io.circe.Codec
    import io.circe.generic.semiauto._
    import com.advancedtelematic.libats.codecs.CirceCodecs._

    implicit val repoNamespaceCodec: Codec[RepoNamespace] = deriveCodec[RepoNamespace]
  }

}
