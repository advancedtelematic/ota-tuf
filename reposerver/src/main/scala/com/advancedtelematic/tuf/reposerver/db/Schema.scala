package com.advancedtelematic.tuf.reposerver.db

import java.time.Instant

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libats.data.Namespace
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, RepoId, SignedPayload}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, TargetItem}
import io.circe.Json
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.StorageMethod._


object Schema {
  import com.advancedtelematic.libats.slick.codecs.SlickRefined._
  import com.advancedtelematic.libats.slick.db.SlickUUIDKey._
  import com.advancedtelematic.libats.slick.db.SlickUriMapper._
  import com.advancedtelematic.libats.slick.db.SlickAnyVal._
  import com.advancedtelematic.libats.slick.db.SlickCirceMapper._
  import com.advancedtelematic.libtuf.data.TufSlickMappings._
  import com.advancedtelematic.libats.slick.db.SlickExtensions.javaInstantMapping

  class TargetItemTable(tag: Tag) extends Table[TargetItem](tag, "target_items") {
    def repoId = column[RepoId]("repo_id")
    def filename = column[TargetFilename]("filename")
    def uri = column[Uri]("uri")
    def custom = column[Option[TargetCustom]]("custom")
    def checksum = column[Checksum]("checksum")
    def length = column[Long]("length")
    def storageMethod = column[StorageMethod]("storage_method")

    def pk = primaryKey("target_items_pk", (repoId, filename))

    override def * = (repoId, filename, uri, checksum, length, custom, storageMethod) <> ((TargetItem.apply _).tupled, TargetItem.unapply)
  }

  protected [db] val targetItems = TableQuery[TargetItemTable]

  class SignedRoleTable(tag: Tag) extends Table[SignedRole](tag, "signed_roles") {
    def repoId = column[RepoId]("repo_id")
    def roleType = column[RoleType]("role_type")
    def content = column[SignedPayload[Json]]("content")
    def checksum = column[Checksum]("checksum")
    def length = column[Long]("length")
    def version = column[Int]("version")
    def expiresAt = column[Instant]("expires_at")

    def pk = primaryKey("signed_role_pk", (repoId, roleType))

    override def * = (repoId, roleType, content, checksum, length, version, expiresAt) <> ((SignedRole.apply _).tupled, SignedRole.unapply)
  }

  protected [db] val signedRoles = TableQuery[SignedRoleTable]

  class RepoNamespaceTable(tag: Tag) extends Table[(RepoId, Namespace)](tag, "repo_namespaces") {
    def repoId = column[RepoId]("repo_id")
    def namespace = column[Namespace]("namespace")

    def pk = primaryKey("repo_namespaces_pk", namespace)

    override def * = (repoId, namespace)
  }

  protected [db] val repoNamespaces = TableQuery[RepoNamespaceTable]
}
