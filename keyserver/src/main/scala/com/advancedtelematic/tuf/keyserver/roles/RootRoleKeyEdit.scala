package com.advancedtelematic.tuf.keyserver.roles

import com.advancedtelematic.libtuf.data.TufDataType.TufPrivateKey
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RepoId}
import com.advancedtelematic.tuf.keyserver.db.{KeyRepositorySupport, RoleRepositorySupport}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultKeyNotFound

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db.KeyRepository.KeyNotFound
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import slick.jdbc.MySQLProfile.api._

class RootRoleKeyEdit(vaultClient: VaultClient)
                      (implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport with RoleRepositorySupport {

  def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[TufPrivateKey] = for {
    _ <- ensureIsRepoKey(repoId, keyId, roleType = None)
    clientPrivateKey <- findParsedPrivateKey(keyId)
    _ <- vaultClient.deleteKey(keyId)
  } yield clientPrivateKey

  private def findParsedPrivateKey(keyId: KeyId): Future[TufPrivateKey] =
    vaultClient.findKey(keyId)
      .map(_.privateKey)
      .recoverWith {
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
