package com.advancedtelematic.tuf.keyserver.roles

import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RepoId, TufKeyPair}
import com.advancedtelematic.tuf.keyserver.db.{KeyRepositorySupport}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db.KeyRepository.KeyNotFound
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultResourceNotFound
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libtuf.data.RootManipulationOps._
import cats.implicits._
import com.advancedtelematic.libtuf.data.RootManipulationOps._
import com.advancedtelematic.tuf.keyserver.http.Errors

class RootRoleKeyEdit(vaultClient: VaultClient)
                      (implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport {
  val roleSigning = new RoleSigning(vaultClient)
  val signedRootRole = new SignedRootRoles(vaultClient)

  def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[Unit] = for {
    _ <- ensureIsRepoKey(repoId, keyId)
    _ <- vaultClient.deleteKey(keyId)
  } yield ()

  def findAllKeyPairs(repoId: RepoId, roleType: RoleType): Future[Seq[TufKeyPair]] = async {
    val root = await(signedRootRole.find(repoId)).signed
    val targetKeyIds = root.roleKeys(roleType).map(_.id)

    val keys = await {
      keyRepo.findAll(targetKeyIds).recoverWith {
        case KeyNotFound =>
          FastFuture.failed(Errors.PrivateKeysNotFound)
      }
    }

    await {
      keys.toList.traverse { key =>
        vaultClient.findKey(key.id).map(_.toTufKeyPair).flatMap(Future.fromTry)
      }
    }
  }

  def findKeyPair(repoId: RepoId, keyId: KeyId): Future[TufKeyPair] = {
    val f = for {
      _ <- ensureIsRepoKey(repoId, keyId)
      vaultKey ← vaultClient.findKey(keyId)
      keyPair ← Future.fromTry(vaultKey.toTufKeyPair)
    } yield keyPair

    f.recoverWith {
      case VaultResourceNotFound => Future.failed(KeyNotFound)
    }
  }

  private def ensureIsRepoKey(repoId: RepoId, keyId: KeyId): Future[KeyId] = async {
    val publicKey = await(keyRepo.repoKeys(repoId)).find(_.id == keyId)
    publicKey.map(_.id).getOrElse(throw KeyNotFound)
  }
}
