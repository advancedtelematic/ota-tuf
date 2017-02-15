package com.advancedtelematic.ota_tuf.db

import java.security.PublicKey

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libtuf.data.TufDataType.KeyType.KeyType
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, KeyId}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.ota_tuf.data.KeyServerDataType._
import com.advancedtelematic.ota_tuf.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.{SignedRole, TargetItem}
import slick.driver.MySQLDriver.api._
import io.circe.Json

object Schema {
  import org.genivi.sota.refined.SlickRefined._
  import SlickPublicKeyMapper._
  import SlickUriMapper._
  import SlickCirceMapper._

  class KeyGenRequestTable(tag: Tag) extends Table[KeyGenRequest](tag, "key_gen_requests") {
    def id = column[KeyGenId]("id", O.PrimaryKey)
    def repoId = column[RepoId]("repo_id")
    def status = column[KeyGenRequestStatus]("status")
    def roleType = column[RoleType]("role_type")
    def keySize = column[Int]("key_size")
    def threshold = column[Int]("threshold")

    def uniqueRepoIdRoleTypeIdx = index("key_gen_requests_unique_idx", (repoId, roleType), unique = true)

    override def * = (id, repoId, status, roleType, keySize, threshold) <> ((KeyGenRequest.apply _).tupled, KeyGenRequest.unapply)
  }

  protected [db] val keyGenRequests = TableQuery[KeyGenRequestTable]

  class KeyTable(tag: Tag) extends Table[Key](tag, "keys") {
    def id = column[KeyId]("key_id", O.PrimaryKey)
    def roleId = column[RoleId]("role_id")
    def keyType = column[KeyType]("key_type")
    def publicKey = column[PublicKey]("public_key")

    def roleFk = foreignKey("keys_role_fk", roleId, roles)(_.id)

    override def * = (id, roleId, keyType, publicKey) <> ((Key.apply _).tupled, Key.unapply)
  }

  protected [db] val keys = TableQuery[KeyTable]

  class RoleTable(tag: Tag) extends Table[Role](tag, "roles") {
    def id = column[RoleId]("role_id", O.PrimaryKey)
    def repoId = column[RepoId]("repo_id")
    def roleType = column[RoleType]("role_type")
    def threshold = column[Int]("threshold")

    def uniqueRepoIdRoleTypeIdx = index("roles_unique_idx", (repoId, roleType), unique = true)

    override def * = (id, repoId, roleType, threshold) <> ((Role.apply _).tupled, Role.unapply)
  }

  protected [db] val roles = TableQuery[RoleTable]

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
