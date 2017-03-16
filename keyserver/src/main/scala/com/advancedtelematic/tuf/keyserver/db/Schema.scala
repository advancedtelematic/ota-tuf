package com.advancedtelematic.tuf.keyserver.db

import java.security.PublicKey
import java.time.Instant

import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.KeyType.KeyType
import com.advancedtelematic.libtuf.data.TufDataType.{ClientSignature, KeyId, RepoId, Signature, SignedPayload, ValidSignature}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.SignatureMethod.SignatureMethod
import eu.timepit.refined.api.Refined
import slick.lifted.ProvenShape

object Schema {
  import com.advancedtelematic.libats.codecs.SlickRefined._
  import com.advancedtelematic.libtuf.data.SlickPublicKeyMapper._
  import com.advancedtelematic.libtuf.data.SlickUriMapper._
  import com.advancedtelematic.libtuf.data.SlickCirceMapper._
  import com.advancedtelematic.libats.db.SlickExtensions.javaInstantMapping

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

  implicit val generatedPayloadRootRoleMapper = circeMapper[RootRole]

  class GeneratedRootRoles(tag: Tag) extends Table[(RepoId, Instant, RootRole)](tag, "generated_root_roles") {
    def repoId = column[RepoId]("repo_id")
    def expiresAt = column[Instant]("expires_at")
    def content = column[RootRole]("content")

    def pk = primaryKey("pk_generated_root_roles", repoId)

    override def * = (repoId, expiresAt, content)
  }

  protected [db] val generatedRootRoles = TableQuery[GeneratedRootRoles]

  // TODO: Reuse Other?
  case class RootSignature(repoId: RepoId, keyId: KeyId, signature: Signature) {
    def toClient = ClientSignature(keyId, signature.method, signature.sig)
  }

  class RootSignaturesTable(tag: Tag) extends Table[RootSignature](tag, "root_signatures") {
    def repoId = column[RepoId]("repo_id")
    def keyId = column[KeyId]("key_id")
    def method = column[SignatureMethod]("method")
    def signature = column[Refined[String, ValidSignature]]("signature")

    def pk = primaryKey("pk_", (repoId, keyId))

    override def * : ProvenShape[RootSignature] = (repoId, keyId, method, signature) <> (
      (f: (RepoId, KeyId, SignatureMethod, Refined[String, ValidSignature])) => RootSignature(f._1, f._2, Signature(f._4, f._3)),
      (o: RootSignature) => Some((o.repoId, o.keyId, o.signature.method, o.signature.sig))
    )
  }

  protected [db] val rootSignatures = TableQuery[RootSignaturesTable]
}
