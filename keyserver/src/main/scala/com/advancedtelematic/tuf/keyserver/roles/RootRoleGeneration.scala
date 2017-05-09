package com.advancedtelematic.tuf.keyserver.roles

import java.time.{Duration, Instant}

import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{KeyGenId, KeyGenRequest, KeyGenRequestStatus}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.tuf.keyserver.http.{Errors, RoleSigning}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import slick.jdbc.MySQLProfile.api._
import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientKey, RoleKeys, RootRole}

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db._


class RootRoleGeneration(vaultClient: VaultClient)
                        (implicit val db: Database, val ec: ExecutionContext)
  extends KeyGenRequestSupport
    with KeyRepositorySupport
    with RootRoleCacheSupport {

  private val DEFAULT_ROLES = RoleType.ALL
  private val DEFAULT_KEY_SIZE = 2048
  private val DEFAULT_ROLE_EXPIRE = Duration.ofDays(365)

  val roleSigning = new RoleSigning(vaultClient)

  def createDefaultGenRequest(repoId: RepoId, threshold: Int): Future[Seq[KeyGenId]] = {
    val reqs = DEFAULT_ROLES.map { roleType =>
      KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, roleType, DEFAULT_KEY_SIZE, threshold)
    }

    keyGenRepo.persistAll(reqs).map(_.map(_.id))
  }

  def findAndCache(repoId: RepoId): Future[SignedPayload[RootRole]] = {
    rootRoleCacheRepo.findCached(repoId).flatMap {
      case Some(signedPayload) => FastFuture.successful(signedPayload)
      case None => signRoot(repoId).flatMap(addToCache(repoId))
    }
  }

  def forceRetry(repoId: RepoId): Future[Seq[KeyGenId]] = {
    keyGenRepo.findBy(repoId).map { genRequests =>
      genRequests.filter(_.status == KeyGenRequestStatus.ERROR).map(_.id)
    }.flatMap { genIds =>
      keyGenRepo.setStatusAll(genIds, KeyGenRequestStatus.REQUESTED)
    }
  }

  private def signRoot(repoId: RepoId): Future[SignedPayload[RootRole]] = {
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
          roleType -> RoleKeys(roleKeys.map(_.id), role.threshold)
      }

      val rootRole = RootRole(clientKeys, roles, expires = Instant.now.plus(DEFAULT_ROLE_EXPIRE), version = 1)

      await(roleSigning.signAll(rootRole, rootKeys))
    }
  }

  private def addToCache(repoId: RepoId)(signedRoot: SignedPayload[RootRole]): Future[SignedPayload[RootRole]] = {
    rootRoleCacheRepo.addCached(repoId, signedRoot).map(_ => signedRoot)
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