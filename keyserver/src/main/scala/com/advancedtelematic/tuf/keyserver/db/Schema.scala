package com.advancedtelematic.tuf.keyserver.db

import java.security.PublicKey
import java.time.Instant

import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.KeyType.KeyType
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RepoId, SignedPayload}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libtuf.data.TufCodecs._

object Schema {
  import com.advancedtelematic.libats.slick.codecs.SlickRefined._
  import com.advancedtelematic.libats.slick.db.SlickUUIDKey._
  import com.advancedtelematic.libats.slick.db.SlickCirceMapper._
  import com.advancedtelematic.libats.slick.db.SlickUriMapper._
  import com.advancedtelematic.libats.slick.db.SlickExtensions._
  import com.advancedtelematic.libtuf.data.TufSlickMappings._

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

  implicit val signedPayloadRootRoleMapper = circeMapper[SignedPayload[RootRole]]

  class SignedRootRoleTable(tag: Tag) extends Table[(RepoId, Instant, Int, SignedPayload[RootRole])](tag, "signed_root_roles") {
    def repoId = column[RepoId]("repo_id")
    def expiresAt = column[Instant]("expires_at")
    def version = column[Int]("version")
    def signedPayload = column[SignedPayload[RootRole]]("signed_payload")

    def pk = primaryKey("pk_signed_root_roles", repoId)

    override def * = (repoId, expiresAt, version, signedPayload)
  }

  protected [db] val signedRootRoles = TableQuery[SignedRootRoleTable]
}
