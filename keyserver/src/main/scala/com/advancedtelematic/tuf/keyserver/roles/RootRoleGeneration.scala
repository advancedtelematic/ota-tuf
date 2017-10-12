package com.advancedtelematic.tuf.keyserver.roles

import java.time.{Duration, Instant}

import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{KeyGenId, KeyGenRequest, KeyGenRequestStatus}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.tuf.keyserver.http.{Errors, RoleSigning}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import slick.jdbc.MySQLProfile.api._
import akka.http.scaladsl.util.FastFuture
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db._

class RootRoleGeneration(vaultClient: VaultClient)
                        (implicit val db: Database, val ec: ExecutionContext)
  extends KeyGenRequestSupport
    with KeyRepositorySupport
    with RoleRepositorySupport
    with SignedRootRoleSupport {

  private val DEFAULT_ROLES = RoleType.ALL
  private val DEFAULT_RSA_KEY_SIZE = 2048
  private val DEFAULT_ROLE_EXPIRE = Duration.ofDays(365)

  val roleSigning = new RoleSigning(vaultClient)

  def createDefaultGenRequest(repoId: RepoId, threshold: Int, keyType: KeyType): Future[Seq[KeyGenId]] = {
    val reqs = DEFAULT_ROLES.map { roleType =>
      KeyGenRequest(KeyGenId.generate(), repoId, KeyGenRequestStatus.REQUESTED, roleType, DEFAULT_RSA_KEY_SIZE, keyType,
        threshold)
    }

    keyGenRepo.persistAll(reqs).map(_.map(_.id))
  }

  def findOrGenerate(repoId: RepoId): Future[SignedPayload[RootRole]] = {
    signedRootRoleRepo.find(repoId).flatMap {
      case Some(signedPayload) => FastFuture.successful(signedPayload)
      case None => forceGenerate(repoId)
    }
  }

  def forceGenerate(repoId: RepoId): Future[SignedPayload[RootRole]] =
    signRoot(repoId).flatMap(persistSigned(repoId))

  def forceRetry(repoId: RepoId): Future[Seq[KeyGenId]] = {
    keyGenRepo.findBy(repoId).map { genRequests =>
      genRequests.filter(_.status == KeyGenRequestStatus.ERROR).map(_.id)
    }.flatMap { genIds =>
      keyGenRepo.setStatusAll(genIds, KeyGenRequestStatus.REQUESTED)
    }
  }

  def createUnsigned(repoId: RepoId): Future[RootRole] = async {
    await(ensureKeysGenerated(repoId))

    val repoKeys = await(keyRepo.repoKeysByRole(repoId))
    val keys = repoKeys.values.flatMap(_._2).toList.distinct

    val clientKeys = keys.map { key =>
      key.id -> key.toTufKey
    }.toMap

    val roles = repoKeys
      .map { case (roleType, (role, roleKeys)) =>
        roleType -> RoleKeys(roleKeys.map(_.id), role.threshold)
      }

    val nextVersion = await(signedRootRoleRepo.nextVersion(repoId))

    RootRole(clientKeys, roles, expires = Instant.now.plus(DEFAULT_ROLE_EXPIRE), version = nextVersion)
  }

  def storeUserSigned(repoId: RepoId, signed: SignedPayload[RootRole]): Future[ValidatedNel[String, SignedPayload[RootRole]]] = {
    roleSigning.newRootIsValid(repoId, signed).flatMap {
      case Valid(newRootRole) => async {
        await(signedRootRoleRepo.storeKeys(repoId, signed.signed))
        await(roleRepo.update(newRootRole))
        Valid(await(persistSigned(repoId)(signed)))
      }
      case r@Invalid(_) =>
        FastFuture.successful(r)
    }
  }

  private def signRoot(repoId: RepoId): Future[SignedPayload[RootRole]] = for {
    rootRole <- createUnsigned(repoId)
    rootKeys <- keyRepo.repoKeysForRole(repoId, RoleType.ROOT)
    signedPayload <- roleSigning.signAll(rootRole, rootKeys.distinct)
  } yield signedPayload

  private def persistSigned(repoId: RepoId)(signedRoot: SignedPayload[RootRole]): Future[SignedPayload[RootRole]] = {
    signedRootRoleRepo.persist(repoId, signedRoot).map(_ => signedRoot)
  }

  private def ensureKeysGenerated(repoId: RepoId): Future[Unit] =
    keyGenRepo.findBy(repoId).flatMap { keyGenReqs =>
      if(keyGenReqs.isEmpty)
        Future.failed(KeyRepository.KeyNotFound)
      else if (keyGenReqs.exists(_.status == KeyGenRequestStatus.ERROR)) {
        val errors = keyGenReqs.foldLeft(Map.empty[KeyGenId, String]) { (errors, req) =>
          if(req.status == KeyGenRequestStatus.ERROR)
            errors + (req.id -> req.description)
          else
            errors
        }
        Future.failed(Errors.KeyGenerationFailed(repoId, errors))
      } else if(keyGenReqs.exists(_.status != KeyGenRequestStatus.GENERATED))
        Future.failed(Errors.KeysNotReady)
      else
        Future.successful(())
    }
}
