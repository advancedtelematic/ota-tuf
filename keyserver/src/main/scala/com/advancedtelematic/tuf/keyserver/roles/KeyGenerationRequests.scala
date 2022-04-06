package com.advancedtelematic.tuf.keyserver.roles

import akka.actor.Scheduler

import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant}
import akka.http.scaladsl.util.FastFuture
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.RootManipulationOps._
import com.advancedtelematic.libtuf.data.RootRoleValidation
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.keyserver.db._
import com.advancedtelematic.tuf.keyserver.http._
import io.circe.syntax._
import org.slf4j.LoggerFactory
import slick.jdbc.MySQLProfile.api._

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}

class SignedRootRoles(defaultRoleExpire: Duration = Duration.ofDays(365))
                     (implicit val db: Database, val ec: ExecutionContext, val scheduler: Scheduler)
extends KeyRepositorySupport with SignedRootRoleSupport {

  private val rootRoleGeneration = new KeyGenerationRequests()
  private val roleSigning = new RoleSigning()

  private val _log = LoggerFactory.getLogger(this.getClass)

  def findByVersion(repoId: RepoId, version: Int): Future[SignedPayload[RootRole]] =
    signedRootRoleRepo.findByVersion(repoId, version).map(_.content)

  def generateNewVersionIfExpired(repoId: RepoId, version: Int): Future[SignedPayload[RootRole]] = {
    findByVersion(repoId, version - 1).flatMap {
      case signedPayload if signedPayload.signed.expires.isBefore(Instant.now.plus(1, ChronoUnit.HOURS)) =>
        generateNewVersion(repoId, signedPayload).recoverWith {
          case Errors.PrivateKeysNotFound =>
            _log.info(s"Could not find private keys to refresh a role for repo $repoId, keys are offline, returning a MissingSignedRole error")
            FastFuture.failed(SignedRootRoleRepository.MissingSignedRole)
        }
      case _ => FastFuture.failed(SignedRootRoleRepository.MissingSignedRole)
    }
  }

  def findForSign(repoId: RepoId): Future[RootRole] =
    find(repoId).map(prepareForSign)

  def findLatest(repoId: RepoId): Future[SignedPayload[RootRole]] =
    find(repoId).map(_.content)

  def findFreshAndPersist(repoId: RepoId): Future[SignedPayload[RootRole]] = async {
    val signedRole = await(findAndPersist(repoId))

    if (signedRole.expiresAt.isBefore(Instant.now.plus(1, ChronoUnit.HOURS))) {
      val f = generateNewVersion(repoId, signedRole.content).recover {
        case Errors.PrivateKeysNotFound =>
          _log.info("Could not find private keys to refresh a role, keys are offline, returning an expired role")
          signedRole.content
      }

      await(f)
    } else {
      signedRole.content
    }
  }

  private def persistSignedPayload(repoId: RepoId)(signedPayload: SignedPayload[RootRole]): Future[SignedRootRole] = {
    val signedRootRole = SignedRootRole.fromSignedPayload(repoId, signedPayload)
    signedRootRoleRepo.persist(signedRootRole).map(_ => signedRootRole)
  }

  protected def find(repoId: RepoId): Future[SignedRootRole] =
    signedRootRoleRepo.findLatest(repoId)

  private def findAndPersist(repoId: RepoId): Future[SignedRootRole] =
    find(repoId).recoverWith {
      case SignedRootRoleRepository.MissingSignedRole =>
        signDefault(repoId).flatMap(persistSignedPayload(repoId))
    }

  def persistUserSigned(repoId: RepoId, offlinePayload: JsonSignedPayload): Future[ValidatedNel[String, SignedRootRole]] = for {
    oldSignedRoot <- signedRootRoleRepo.findLatest(repoId).map(_.content)
    offlineSignedParsedV = userSignedJsonIsValid(offlinePayload, oldSignedRoot)
    userSignedIsValid <- offlineSignedParsedV match {
      case Valid(offlineSignedParsed) =>
        val newOnlineKeys = offlineSignedParsed.signed.keys.values.map(_.id).toSet
        val signedRootRole = SignedRootRole.fromSignedPayload(repoId, offlineSignedParsed)
        signedRootRoleRepo.persistAndKeepRepoKeys(keyRepo)(signedRootRole, newOnlineKeys).map(_ => Valid(signedRootRole))

      case r@Invalid(_) =>
        FastFuture.successful(r)
    }
  } yield userSignedIsValid

  private def userSignedJsonIsValid(offlinePayload: JsonSignedPayload, existingSignedRoot: SignedPayload[RootRole]): ValidatedNel[String, SignedPayload[RootRole]] = {
    RootRoleValidation.rootRawJsonIsValid(offlinePayload).andThen { offlineSignedParsed =>
      RootRoleValidation.newRootIsValid(offlineSignedParsed, existingSignedRoot)
    }
  }

  private def signDefault(repoId: RepoId): Future[SignedPayload[RootRole]] =
    createDefault(repoId).flatMap(signRootRole)

  private def signRootRole(role: RootRole): Future[SignedPayload[RootRole]] = async {
    val keys = role.roleKeys(RoleType.ROOT)
    val payloadJson = role.asJson
    val signatures = await(roleSigning.signWithKeys(payloadJson, keys))
    SignedPayload(signatures, role, payloadJson)
  }

  private def prepareForSign(signedRootRole: SignedRootRole): RootRole =
    signedRootRole.content.signed.copy(expires = Instant.now.plus(defaultRoleExpire), version = signedRootRole.version + 1)

  private def ensureReadyForGenerate(repoId: RepoId): Future[Unit] =
    keyRepo.repoKeys(repoId).flatMap {
      case keys if keys.exists(_.roleType == RoleType.ROOT) => FastFuture.successful(())
      case _ => FastFuture.failed(Errors.RepoRootKeysNotFound)
    }

  private def createDefault(repoId: RepoId): Future[RootRole] = async {
    val keyGenRequests = await(rootRoleGeneration.readyKeyGenRequests(repoId))
    await(ensureReadyForGenerate(repoId))

    val repoKeys = await(keyRepo.repoKeys(repoId)).toSet

    val clientKeys = repoKeys.map { key => key.id -> key.publicKey }.toMap

    val roleTypeToKeyIds = repoKeys.groupBy(_.roleType).mapValues(_.map(_.id).toSeq)

    val roles = keyGenRequests.map { genRequest =>
      genRequest.roleType → RoleKeys(roleTypeToKeyIds(genRequest.roleType), genRequest.threshold)
    }.toMap

    assert(clientKeys.nonEmpty, "no keys for new default root")
    assert(roles.nonEmpty, "no roles for new default root")

    RootRole(clientKeys, roles, expires = Instant.now.plus(defaultRoleExpire), version = 1)
  }

  private def generateNewVersion(repoId: RepoId, signedPayload: SignedPayload[RootRole]): Future[SignedPayload[RootRole]] = {
    val versionedRole = signedPayload.signed
    val nextVersion = versionedRole.version + 1
    val nextExpires = Instant.now.plus(defaultRoleExpire)
    val newRole = versionedRole.copy(expires = nextExpires, version = nextVersion)
    signRootRole(newRole).flatMap(persistSignedPayload(repoId)).map(_.content).recoverWith {
      case SignedRootRoleRepository.RootRoleExists => findByVersion(repoId, nextVersion)
    }
  }
}

class KeyGenerationRequests()
                           (implicit val db: Database, val ec: ExecutionContext, val scheduler: Scheduler)
  extends KeyGenRequestSupport with KeyRepositorySupport {

  private val DEFAULT_ROLES = RoleType.ALL

  def createDefaultGenRequest(repoId: RepoId, threshold: Int, keyType: KeyType, initStatus: KeyGenRequestStatus): Future[Seq[KeyGenRequest]] = {
    val reqs = DEFAULT_ROLES.map { roleType =>
      KeyGenRequest(KeyGenId.generate(), repoId, initStatus, roleType, keyType.crypto.defaultKeySize, keyType, threshold)
    }

    keyGenRepo.persistAll(reqs)
  }

  def forceRetry(repoId: RepoId): Future[Seq[KeyGenId]] = {
    keyGenRepo.findBy(repoId).map { genRequests =>
      genRequests.filter(_.status == KeyGenRequestStatus.ERROR).map(_.id)
    }.flatMap { genIds =>
      keyGenRepo.setStatusAll(genIds, KeyGenRequestStatus.REQUESTED)
    }
  }

  def readyKeyGenRequests(repoId: RepoId): Future[Seq[KeyGenRequest]] =
    keyGenRepo.findBy(repoId).flatMap { keyGenReqs =>
      if(keyGenReqs.isEmpty)
        FastFuture.failed(KeyRepository.KeyNotFound)
      else if (keyGenReqs.exists(_.status == KeyGenRequestStatus.ERROR)) {
        val errors = keyGenReqs.foldLeft(Map.empty[KeyGenId, String]) { (errors, req) =>
          if(req.status == KeyGenRequestStatus.ERROR)
            errors + (req.id -> req.description)
          else
            errors
        }
        FastFuture.failed(Errors.KeyGenerationFailed(repoId, errors))
      } else if(keyGenReqs.exists(_.status != KeyGenRequestStatus.GENERATED))
        FastFuture.failed(Errors.KeysNotReady)
      else
        FastFuture.successful(keyGenReqs)
    }
}
