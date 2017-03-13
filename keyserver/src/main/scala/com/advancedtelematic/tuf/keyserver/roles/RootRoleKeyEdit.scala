package com.advancedtelematic.tuf.keyserver.roles

import java.security.PrivateKey

import com.advancedtelematic.libtuf.data.ClientDataType.ClientPrivateKey
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, KeyType, RepoId, RoleType}
import com.advancedtelematic.tuf.keyserver.db.{KeyRepositorySupport, RoleRepositorySupport}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultKeyNotFound
import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db.KeyRepository.KeyNotFound
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import slick.driver.MySQLDriver.api._

class RootRoleKeyEdit(vaultClient: VaultClient)
                      (implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport with RoleRepositorySupport {
  def fetchPrivateKey(repoId: RepoId, keyId: KeyId): Future[ClientPrivateKey] = async {
    val rootPublicKey = await(ensureIsRepoRootKey(repoId, keyId))

    val privateKey = await(findParsedPrivateKey(rootPublicKey))

    ClientPrivateKey(KeyType.RSA, privateKey)
  }

  def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[ClientPrivateKey] = async {
    val rootPublicKey = await(ensureIsRepoRootKey(repoId, keyId))

    val privateKey = await(findParsedPrivateKey(rootPublicKey))

    await(vaultClient.deleteKey(rootPublicKey))

    ClientPrivateKey(KeyType.RSA, privateKey)
  }

  private def findParsedPrivateKey(keyId: KeyId): Future[PrivateKey] =
    vaultClient.findKey(keyId).flatMap { vaultKey =>
      Future.fromTry(RsaKeyPair.parseKeyPair(vaultKey.privateKey).map(_.getPrivate))
    }.recoverWith {
      case VaultKeyNotFound => Future.failed(KeyNotFound)
    }

  private def ensureIsRepoRootKey(repoId: RepoId, keyId: KeyId): Future[KeyId] = async {
    val rootPublicKey = await(keyRepo.repoKeys(repoId, RoleType.ROOT)).find(_.id == keyId)
    rootPublicKey.map(_.id).getOrElse(throw KeyNotFound)
  }
}
