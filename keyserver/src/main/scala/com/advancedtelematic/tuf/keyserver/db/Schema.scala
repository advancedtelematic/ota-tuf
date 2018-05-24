package com.advancedtelematic.tuf.keyserver.db

import java.security.PublicKey
import java.time.Instant

import com.advancedtelematic.libats.slick.db.SlickEncryptedColumn.EncryptedColumn
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, KeyType, RepoId, SignedPayload, TufPrivateKey}
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
  import com.advancedtelematic.libtuf_server.data.TufSlickMappings._
  import SlickMappings._

  class KeyGenRequestTable(tag: Tag) extends Table[KeyGenRequest](tag, "key_gen_requests") {
    def id = column[KeyGenId]("id", O.PrimaryKey)
    def repoId = column[RepoId]("repo_id")
    def status = column[KeyGenRequestStatus]("status")
    def roleType = column[RoleType]("role_type")
    def keyType = column[KeyType]("key_type")
    def keySize = column[Int]("key_size")
    def threshold = column[Int]("threshold")
    def description = column[String]("description")

    def uniqueRepoIdRoleTypeIdx = index("key_gen_requests_unique_idx", (repoId, roleType), unique = true)

    override def * = (id, repoId, status, roleType, keySize, keyType, threshold, description) <> ((KeyGenRequest.apply _).tupled, KeyGenRequest.unapply)
  }

  protected [db] val keyGenRequests = TableQuery[KeyGenRequestTable]

  class KeyTable(tag: Tag) extends Table[Key](tag, "keys") {
    def id = column[KeyId]("key_id", O.PrimaryKey)
    def repoId = column[RepoId]("repo_id")
    def roleType = column[RoleType]("role_type")
    def keyType = column[KeyType]("key_type")
    def publicKey = column[PublicKey]("public_key") // TODO: Use TufKey instead, migrate, remove `key_type`, then remove KeyType Enum ?
    def privateKey = column[EncryptedColumn[TufPrivateKey]]("private_key")

    override def * = (id, repoId, roleType, keyType, publicKey, privateKey.decrypted) <> ((Key.apply _).tupled, Key.unapply)
  }

  protected [db] val keys = TableQuery[KeyTable]

  implicit val signedPayloadRootRoleMapper = circeMapper[SignedPayload[RootRole]]

  class SignedRootRoleTable(tag: Tag) extends Table[(RepoId, Instant, Int, SignedPayload[RootRole])](tag, "signed_root_roles") {
    def repoId = column[RepoId]("repo_id")
    def expiresAt = column[Instant]("expires_at")
    def version = column[Int]("version")
    def signedPayload = column[SignedPayload[RootRole]]("signed_payload")

    def pk = primaryKey("pk_signed_root_roles", (repoId, version))

    override def * = (repoId, expiresAt, version, signedPayload)
  }

  protected [db] val signedRootRoles = TableQuery[SignedRootRoleTable]
}
