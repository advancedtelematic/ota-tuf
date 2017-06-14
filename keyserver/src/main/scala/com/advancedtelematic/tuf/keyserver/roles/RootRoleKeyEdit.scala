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
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import slick.jdbc.MySQLProfile.api._

class RootRoleKeyEdit(vaultClient: VaultClient)
                      (implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport with RoleRepositorySupport {

  def fetchPrivateKey(repoId: RepoId, keyId: KeyId): Future[ClientPrivateKey] = for {
    rootPublicKey <- ensureIsRepoKey(repoId, keyId, Some(RoleType.ROOT))
    privateKey <- findParsedPrivateKey(rootPublicKey)
  } yield ClientPrivateKey(KeyType.RSA, privateKey)

  def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[ClientPrivateKey] = for {
    _ <- ensureIsRepoKey(repoId, keyId, roleType = None)
    privateKey <- findParsedPrivateKey(keyId)
    _ <- vaultClient.deleteKey(keyId)
  } yield ClientPrivateKey(KeyType.RSA, privateKey)

  private def findParsedPrivateKey(keyId: KeyId): Future[PrivateKey] =
    vaultClient.findKey(keyId).flatMap { vaultKey =>
      Future.fromTry(RsaKeyPair.parseKeyPair(vaultKey.privateKey).map(_.getPrivate))
    }.recoverWith {
      case VaultKeyNotFound => Future.failed(KeyNotFound)
    }

  private def ensureIsRepoKey(repoId: RepoId, keyId: KeyId, roleType: Option[RoleType]): Future[KeyId] = async {
    val repoKeysF = roleType match {
      case Some(rt) => keyRepo.repoKeysForRole(repoId, rt)
      case None => keyRepo.repoKeys(repoId)
    }

    val publicKey = await(repoKeysF).find(k => k.id == keyId)
    publicKey.map(_.id).getOrElse(throw KeyNotFound)
  }
}
