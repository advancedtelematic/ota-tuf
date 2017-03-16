package com.advancedtelematic.tuf.keyserver.roles

import java.time.{Duration, Instant}

import cats.data.NonEmptyList
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{KeyGenId, KeyGenRequest, KeyGenRequestStatus}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.tuf.keyserver.http.{Errors, PrivateKeysFetch, RoleSigning}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientKey, RoleKeys, RootRole}
import com.advancedtelematic.tuf.keyserver.db.Schema.RootSignature

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db._

class RootRoleSigning(vaultClient: VaultClient)
                     (implicit val db: Database, val ec: ExecutionContext)
extends KeyRepositorySupport with GeneratedRootRolesSupport with RootSignaturesSupport {

  val privateKeysFetch = new PrivateKeysFetch(vaultClient)
  val roleSigning = new RoleSigning() // TODO: Use object

  def persistSignaturesFromAvailableKeys(repoId: RepoId, rootRole: RootRole): Future[Seq[RootSignature]] = {
    async {
      // await(ensureKeysGenerated(repoId)) // TODO: Ensure signatures exist

      val repoKeysF = keyRepo.repoKeys(repoId, RoleType.ROOT)
      val repoPrivateKeys = await(repoKeysF.flatMap(privateKeysFetch.fetchPrivateKeys))

      val privateKeys = NonEmptyList.fromListUnsafe(repoPrivateKeys) // TODO: Unsafe

      val signedPayload = roleSigning.signAll(rootRole, privateKeys)

      val rootSignatures = signedPayload.signatures.map { sig =>
        RootSignature(repoId, sig.keyid, sig.toSignature)
      }

      await(rootSignaturesRepo.persist(rootSignatures))
    }
  }

  def signatures(repoId: RepoId): Future[Seq[ClientSignature]] = {
    rootSignaturesRepo.find(repoId).map(_.map(_.toClient))
  }
}

class RootRoleGeneration()
                        (implicit val db: Database, val ec: ExecutionContext)
  extends KeyGenRequestSupport
    with KeyRepositorySupport
    with GeneratedRootRolesSupport {

  private val DEFAULT_ROLES = RoleType.ALL
  private val DEFAULT_KEY_SIZE = 2048
  private val DEFAULT_ROLE_EXPIRE = Duration.ofDays(365)

  def createDefaultGenRequest(repoId: RepoId, threshold: Int): Future[Seq[KeyGenId]] = {
    val reqs = DEFAULT_ROLES.map { roleType =>
      KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, roleType, DEFAULT_KEY_SIZE, threshold)
    }

    keyGenRepo.persistAll(reqs).map(_.map(_.id))
  }

  def generate(repoId: RepoId): Future[RootRole] = async {
    await(ensureKeysGenerated(repoId))

    val repoKeys = await(keyRepo.repoKeysByRole(repoId))

    val keys = repoKeys.values.flatMap(_._2).toList.distinct

    val clientKeys = keys.map { key =>
      key.id -> ClientKey(key.keyType, key.publicKey)
    }.toMap

    val roles = repoKeys
      .map { case (roleType, (role, roleKeys)) =>
        roleType -> RoleKeys(roleKeys.map(_.id), role.threshold)
      }

    // TODO: Increase version
    val rootRole = RootRole(clientKeys, roles, expires = Instant.now.plus(DEFAULT_ROLE_EXPIRE), version = 1)

    rootRole
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