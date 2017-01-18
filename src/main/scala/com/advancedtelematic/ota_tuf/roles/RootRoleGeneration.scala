package com.advancedtelematic.ota_tuf.roles

import cats.syntax.show.toShowOps
import com.advancedtelematic.ota_tuf.data.ClientDataType._
import com.advancedtelematic.ota_tuf.data.Codecs._
import com.advancedtelematic.ota_tuf.data.DataType.{GroupId, KeyGenId, KeyGenRequest}
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

  def createDefaultGenRequest(groupId: GroupId, threshold: Int): Future[Seq[KeyGenId]] = {
    val reqs = DEFAULT_ROLES.map { roleType =>
      KeyGenRequest(KeyGenId.generate(), groupId, KeyGenRequestStatus.REQUESTED, roleType, DEFAULT_KEY_SIZE, threshold)
    }

    keyGenRepo.persistAll(reqs).map(_.map(_.id))
  }

  def findSigned(groupId: GroupId): Future[SignedPayload[RootRole]] = {
    async {
      await(ensureKeysGenerated(groupId))

      val groupKeys = await(keyRepo.groupKeysByRole(groupId))

      val keys = groupKeys.values.flatMap(_._2).toList.distinct

      val clientKeys = keys.map { key =>
        key.id -> ClientKey(key.keyType, key.publicKey)
      }.toMap

      val roles = groupKeys
        .map { case (roleType, (role, roleKeys)) =>
          roleType.show -> RoleKeys(roleKeys.map(_.id), role.threshold)
      }

      val rootRole = RootRole(clientKeys, roles, version = 1)

      await(roleSigning.signAll(rootRole, keys))
    }
  }

  private def ensureKeysGenerated(groupId: GroupId): Future[Unit] =
    keyGenRepo.findBy(groupId).flatMap { keyGenReqs =>
      if(keyGenReqs.isEmpty)
        Future.failed(KeyRepository.KeyNotFound)
      else if(keyGenReqs.exists(_.status != KeyGenRequestStatus.GENERATED))
        Future.failed(Errors.KeysNotReady)
      else
        Future.successful(())
    }
}
