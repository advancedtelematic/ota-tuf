package com.advancedtelematic.tuf.reposerver.db

import java.security.PublicKey

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libtuf.data.TufDataType.KeyType.KeyType
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, KeyId, RepoId}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, TargetItem}
import io.circe.Json
import slick.driver.MySQLDriver.api._

object Schema {
  import com.advancedtelematic.libats.codecs.SlickRefined._
  import com.advancedtelematic.libtuf.data.SlickCirceMapper._
  import com.advancedtelematic.libtuf.data.SlickPublicKeyMapper._
  import com.advancedtelematic.libtuf.data.SlickUriMapper._

  class TargetItemTable(tag: Tag) extends Table[TargetItem](tag, "target_items") {
    def repoId = column[RepoId]("repo_id")
    def filename = column[String]("filename")
    def uri = column[Uri]("uri")
    def checksum = column[Checksum]("checksum")
    def length = column[Long]("length")

    def pk = primaryKey("target_items_pk", (repoId, filename))

    override def * = (repoId, filename, uri, checksum, length) <> ((TargetItem.apply _).tupled, TargetItem.unapply)
  }

  protected [db] val targetItems = TableQuery[TargetItemTable]

  class SignedRoleTable(tag: Tag) extends Table[SignedRole](tag, "signed_roles") {
    def repoId = column[RepoId]("repo_id")
    def roleType = column[RoleType]("role_type")
    def content = column[Json]("content")
    def checksum = column[Checksum]("checksum")
    def length = column[Long]("length")
    def version = column[Int]("version")

    def pk = primaryKey("signed_role_pk", (repoId, roleType))

    override def * = (repoId, roleType, content, checksum, length, version) <> ((SignedRole.apply _).tupled, SignedRole.unapply)
  }

  protected [db] val signedRoles = TableQuery[SignedRoleTable]
}
