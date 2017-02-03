package com.advancedtelematic.ota_tuf.roles

import cats.syntax.show.toShowOps
import com.advancedtelematic.ota_tuf.data.ClientDataType._
import com.advancedtelematic.ota_tuf.data.Codecs._
import com.advancedtelematic.ota_tuf.data.DataType.{RepoId, KeyGenId, KeyGenRequest}
import com.advancedtelematic.ota_tuf.data.RoleType.show
import com.advancedtelematic.ota_tuf.data.{KeyGenRequestStatus, RoleType}
import com.advancedtelematic.ota_tuf.db.{KeyGenRequestSupport, KeyRepository, KeyRepositorySupport}
import com.advancedtelematic.ota_tuf.http.{Errors, RoleSigning}
import com.advancedtelematic.ota_tuf.vault.VaultClient
import slick.driver.MySQLDriver.api._

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}

class RootRoleGeneration(vaultClient: VaultClient)
                        (implicit val db: Database, val ec: ExecutionContext)
  extends KeyGenRequestSupport with KeyRepositorySupport {

  private val DEFAULT_ROLES = RoleType.ALL
  private val DEFAULT_KEY_SIZE = 1024

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

      val clientKeys = keys.map { key =>
        key.id -> ClientKey(key.keyType, key.publicKey)
      }.toMap

      val roles = repoKeys
        .map { case (roleType, (role, roleKeys)) =>
          roleType.show -> RoleKeys(roleKeys.map(_.id), role.threshold)
      }

      val rootRole = RootRole(clientKeys, roles, version = 1)

      await(roleSigning.signAll(rootRole, keys))
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
