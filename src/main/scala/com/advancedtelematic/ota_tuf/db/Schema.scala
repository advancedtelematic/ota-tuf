package com.advancedtelematic.ota_tuf.db

import java.security.PublicKey

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.ota_tuf.data.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.data.KeyType.KeyType
import com.advancedtelematic.ota_tuf.data.RoleType.RoleType
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.{Checksum, SignedRole, TargetItem}
import io.circe.Json

object Schema {
  import com.advancedtelematic.ota_tuf.data.DataType._
  import org.genivi.sota.refined.SlickRefined._
  import SlickPublicKeyMapper._
  import SlickUriMapper._
  import SlickCirceMapper._

  class KeyGenRequestTable(tag: Tag) extends Table[KeyGenRequest](tag, "key_gen_requests") {
    def id = column[KeyGenId]("id", O.PrimaryKey)
    def groupId = column[GroupId]("group_id")
    def status = column[KeyGenRequestStatus]("status")
    def roleType = column[RoleType]("role_type")
    def keySize = column[Int]("key_size")
    def threshold = column[Int]("threshold")

    def uniqueGroupIdRoleTypeIdx = index("key_gen_requests_unique_idx", (groupId, roleType), unique = true)

    override def * = (id, groupId, status, roleType, keySize, threshold) <> ((KeyGenRequest.apply _).tupled, KeyGenRequest.unapply)
  }

  protected [db] val keyGenRequests = TableQuery[KeyGenRequestTable]

  class KeyTable(tag: Tag) extends Table[Key](tag, "keys") {
    def id = column[KeyId]("key_id")
    def roleId = column[RoleId]("role_id")
    def keyType = column[KeyType]("key_type")
    def publicKey = column[PublicKey]("public_key")

    def roleFk = foreignKey("keys_role_fk", roleId, roles)(_.id)

    override def * = (id, roleId, keyType, publicKey) <> ((Key.apply _).tupled, Key.unapply)
  }

  protected [db] val keys = TableQuery[KeyTable]

  class RoleTable(tag: Tag) extends Table[Role](tag, "roles") {
    def id = column[RoleId]("role_id", O.PrimaryKey)
    def groupId = column[GroupId]("group_id")
    def roleType = column[RoleType]("role_type")
    def threshold = column[Int]("threshold")

    def uniqueGroupIdRoleTypeIdx = index("roles_unique_idx", (groupId, roleType), unique = true)

    override def * = (id, groupId, roleType, threshold) <> ((Role.apply _).tupled, Role.unapply)
  }

  protected [db] val roles = TableQuery[RoleTable]

  class TargetItemTable(tag: Tag) extends Table[TargetItem](tag, "target_items") {
    def groupId = column[GroupId]("group_id")
    def filename = column[String]("filename")
    def uri = column[Uri]("uri")
    def checksum = column[Checksum]("checksum")
    def length = column[Long]("length")

    def pk = primaryKey("target_items_pk", (groupId, filename))

    override def * = (groupId, filename, uri, checksum, length) <> ((TargetItem.apply _).tupled, TargetItem.unapply)
  }

  protected [db] val targetItems = TableQuery[TargetItemTable]

  class SignedRoleTable(tag: Tag) extends Table[SignedRole](tag, "signed_roles") {
    def groupId = column[GroupId]("group_id")
    def roleType = column[RoleType]("role_type")
    def content = column[Json]("content")
    def checksum = column[Checksum]("checksum")
    def length = column[Long]("length")

    def pk = primaryKey("signed_role_pk", (groupId, roleType))

    override def * = (groupId, roleType, content, checksum, length) <> ((SignedRole.apply _).tupled, SignedRole.unapply)
  }

  protected [db] val signedRoles = TableQuery[SignedRoleTable]
}
