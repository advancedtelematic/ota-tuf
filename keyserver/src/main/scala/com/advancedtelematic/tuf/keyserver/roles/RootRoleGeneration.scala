package com.advancedtelematic.tuf.keyserver.roles

import java.time.{Duration, Instant}

import cats.syntax.show.toShowOps
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, KeyGenId, KeyGenRequest, KeyGenRequestStatus}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.tuf.keyserver.http.{Errors, RoleSigning}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import slick.driver.MySQLDriver.api._
import RoleType.show
import com.advancedtelematic.tuf.keyserver.db.KeyRepository.KeyNotFound
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientKey, ClientPrivateKey, RoleKeys, RootRole}

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepository, KeyRepositorySupport}

class RootRoleGeneration(vaultClient: VaultClient)
                        (implicit val db: Database, val ec: ExecutionContext)
  extends KeyGenRequestSupport with KeyRepositorySupport {

  private val DEFAULT_ROLES = RoleType.ALL
  private val DEFAULT_KEY_SIZE = 2048
  private val DEFAULT_ROLE_EXPIRE = Duration.ofDays(31)

  val roleSigning = new RoleSigning(vaultClient)

  def createDefaultGenRequest(repoId: RepoId, threshold: Int): Future[Seq[KeyGenId]] = {
    val reqs = DEFAULT_ROLES.map { roleType =>
      KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, roleType, DEFAULT_KEY_SIZE, threshold)
    }

    keyGenRepo.persistAll(reqs).map(_.map(_.id))
  }

  def findSigned(repoId: RepoId): Future[SignedPayload[RootRole]] = {
    async {
      await(ensureKeysGenerated(repoId))

      val repoKeys = await(keyRepo.repoKeysByRole(repoId))

      val keys = repoKeys.values.flatMap(_._2).toList.distinct

      val rootKeys = repoKeys.get(RoleType.ROOT).toList.flatMap(_._2).distinct

      val clientKeys = keys.map { key =>
        key.id -> ClientKey(key.keyType, key.publicKey)
      }.toMap

      val roles = repoKeys
        .map { case (roleType, (role, roleKeys)) =>
          roleType.show -> RoleKeys(roleKeys.map(_.id), role.threshold)
      }

      val rootRole = RootRole(clientKeys, roles, expires = Instant.now.plus(DEFAULT_ROLE_EXPIRE), version = 1)

      await(roleSigning.signAll(rootRole, rootKeys))
    }
  }

  def fetchPrivateKey(repoId: RepoId, keyId: KeyId): Future[ClientPrivateKey] = async {
    val rootPublicKey = await(keyRepo.repoKeys(repoId, RoleType.ROOT)).find(_.id == keyId)

    val publicKeyId = rootPublicKey.map(_.id).getOrElse(throw KeyNotFound)

    val vaultKey = await(vaultClient.findKey(publicKeyId))

    val pk = RsaKeyPair.parseKeyPair(vaultKey.privateKey).get.getPrivate

    ClientPrivateKey(KeyType.RSA, pk)
  }

  private def ensureKeysGenerated(repoId: RepoId): Future[Unit] =
    keyGenRepo.findBy(repoId).flatMap { keyGenReqs =>
      if(keyGenReqs.isEmpty)
        Future.failed(KeyRepository.KeyNotFound)
      else if(keyGenReqs.exists(_.status != KeyGenRequestStatus.GENERATED))
        Future.failed(Errors.KeysNotReady)
      else
        Future.successful(())
    }
}