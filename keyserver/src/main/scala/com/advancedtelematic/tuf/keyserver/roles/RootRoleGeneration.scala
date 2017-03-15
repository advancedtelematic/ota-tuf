package com.advancedtelematic.tuf.keyserver.roles

import java.time.{Duration, Instant}

import cats.data.NonEmptyList
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.tuf.keyserver.http.{Errors, PrivateKeysFetch, RoleSigning}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientKey, ClientPrivateKey, RoleKeys, RootRole}

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db._


class RootRoleGeneration(vaultClient: VaultClient)
                        (implicit val db: Database, val ec: ExecutionContext)
  extends KeyGenRequestSupport
    with KeyRepositorySupport
    with RoleRepositorySupport
    with SignedRootRolesSupport {

  private val DEFAULT_ROLES = RoleType.ALL
  private val DEFAULT_KEY_SIZE = 2048
  private val DEFAULT_ROLE_EXPIRE = Duration.ofDays(365)

  val roleSigning = new RoleSigning()
  val privateKeyFetch = new PrivateKeysFetch(vaultClient)

  def createDefaultGenRequest(repoId: RepoId, threshold: Int): Future[Seq[KeyGenId]] = {
    val reqs = DEFAULT_ROLES.map { roleType =>
      KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, roleType, DEFAULT_KEY_SIZE, threshold)
    }

    keyGenRepo.persistAll(reqs).map(_.map(_.id))
  }

  def signRoot(repoId: RepoId): Future[SignedPayload[RootRole]] = {
    async {
      await(ensureKeysGenerated(repoId))

      val repoKeys = NonEmptyList.fromListUnsafe(await(keyRepo.repoKeysByRole(repoId)).get(RoleType.ROOT).get._2.toList)

      val privateKeys = NonEmptyList.fromListUnsafe(await(privateKeyFetch.fetchPrivateKeys(repoKeys.toList)))

      await(signWithKeys(repoId, repoKeys, privateKeys))
    }
  }

  def signWithKeys(repoId: RepoId, publicRootKeys: NonEmptyList[Key], privateRootKeys: NonEmptyList[TufPrivateKey]): Future[SignedPayload[RootRole]] = {
    async {
      await(ensureKeysGenerated(repoId))

      // TODO: Simplify this, just get all keys, then get the role
      val repoKeys = await(keyRepo.repoKeysByRole(repoId))

      val keys = (repoKeys.values.flatMap(_._2).toList ++ publicRootKeys.toList).distinct

      val clientKeys = keys.map { key =>
        key.id -> ClientKey(key.keyType, key.publicKey)
      }.toMap

      val roles = repoKeys
        .map {
          case (roleType, (role, roleKeys)) =>
            roleType -> RoleKeys(roleKeys.map(_.id), role.threshold)
        }

      val rootRoleDB = await(roleRepo.findByType(repoId, RoleType.ROOT))
      val publicRootKeyIds = publicRootKeys.map(_.id).toList
      val wat = roles ++ Map(RoleType.ROOT -> RoleKeys(publicRootKeyIds, rootRoleDB.threshold))

      val rootRole = RootRole(clientKeys, wat, expires = Instant.now.plus(DEFAULT_ROLE_EXPIRE), version = 1)

      roleSigning.signAll(rootRole, privateRootKeys)
    }
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