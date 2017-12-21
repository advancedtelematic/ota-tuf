package com.advancedtelematic.tuf.keyserver.roles

import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RepoId, RoleType, SignedPayload, TufKey, TufKeyPair, TufPrivateKey}
import com.advancedtelematic.tuf.keyserver.db.{KeyRepositorySupport, RoleRepositorySupport}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultKeyNotFound

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db.KeyRepository.KeyNotFound
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.Key
import slick.jdbc.MySQLProfile.api._

class RootRoleKeyEdit(vaultClient: VaultClient)
                      (implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport with RoleRepositorySupport {

  val rootRoleGeneration = new RootRoleGeneration(vaultClient)

  def addPublicKey(repoId: RepoId, roleType: RoleType, key: TufKey): Future[SignedPayload[RootRole]] = async {
    require(roleType == RoleType.TARGETS, s"Adding keys to roleType $roleType not supported. Only TARGETS is supported")
    val roleId = await(roleRepo.find(repoId, roleType)).id
    await(keyRepo.persist(Key(key.id, roleId, key.keytype, key.keyval)))
    await(rootRoleGeneration.forceGenerate(repoId))
  }

  def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[Unit] = for {
    _ <- ensureIsRepoKey(repoId, keyId, roleType = None)
    _ <- vaultClient.deleteKey(keyId)
  } yield ()

  def findKeyPair(repoId: RepoId, keyId: KeyId): Future[TufPrivateKey] = {
    val f = for {
      _ <- ensureIsRepoKey(repoId, keyId, roleType = None)
      vaultKey ← vaultClient.findKey(keyId)
    } yield vaultKey.privateKey

    f.recoverWith {
      case VaultKeyNotFound => Future.failed(KeyNotFound)
    }
  }

  private def ensureIsRepoKey(repoId: RepoId, keyId: KeyId, roleType: Option[RoleType]): Future[KeyId] = async {
    val repoKeysF = roleType match {
      case Some(rt) => keyRepo.repoKeysForRole(repoId, rt)
      case None => keyRepo.repoKeys(repoId)
    }

    val publicKey = await(repoKeysF).find(_.id == keyId)
    publicKey.map(_.id).getOrElse(throw KeyNotFound)
  }
}
