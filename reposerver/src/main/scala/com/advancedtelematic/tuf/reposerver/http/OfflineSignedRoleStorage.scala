package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.util.FastFuture
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RepoId, RoleType, SignedPayload}
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.SignedRole
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepositorySupport
import io.circe.Encoder
import cats.implicits._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import slick.jdbc.MySQLProfile.api._

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}

class OfflineSignedRoleStorage(keyserverClient: KeyserverClient)
                                        (implicit val db: Database, val ec: ExecutionContext) extends SignedRoleRepositorySupport {

  def store(repoId: RepoId, signedPayload: SignedPayload[TargetsRole]): Future[ValidatedNel[String, SignedPayload[TargetsRole]]] =
    payloadSignatureIsValid(repoId, signedPayload).flatMap {
      case v @ Valid(_) =>
        val signedRole = SignedRole.withChecksum(repoId, RoleType.TARGETS, signedPayload, signedPayload.signed.version)
        signedRoleRepo.persist(signedRole).map(_ => v)
      case i @ Invalid(_) =>
        FastFuture.successful(i)
    }

  private def fetchRootRole(repoId: RepoId): Future[RootRole] = {
    keyserverClient.fetchRootRole(repoId).flatMap { rootRoleJson =>
      Future.fromTry {
        rootRoleJson.signed.as[RootRole].toTry
      }
    }
  }

  // TODO: DUplication, see RoleSigning
  private def payloadSignatureIsValid[T : Encoder](repoId: RepoId, signedPayload: SignedPayload[T]): Future[ValidatedNel[String, SignedPayload[T]]] = async {
    val rootRole = await(fetchRootRole(repoId))

    val publicKeys = rootRole.keys.filterKeys(keyId => rootRole.roles(RoleType.TARGETS).keyids.contains(keyId))

    val sigsByKeyId = signedPayload.signatures.map(s => s.keyid -> s).toMap

    val validSignatureCount: ValidatedNel[String, List[KeyId]] =
      sigsByKeyId.par.map { case (keyId, sig) =>
        publicKeys.get(keyId) match {
          case Some(key) =>
            if (TufCrypto.isValid(signedPayload.signed, sig, key.keyval))
              Valid(keyId)
            else
              Invalid(NonEmptyList.of(s"Invalid signature for key ${sig.keyid}"))
          case None =>
            Invalid(NonEmptyList.of(s"No public key available for key ${sig.keyid}"))
        }
      }.toList.sequenceU

    val threshold = rootRole.roles(RoleType.TARGETS).threshold

    validSignatureCount andThen { validSignatures =>
      if (validSignatures.size >= threshold && threshold > 0)
        Valid(signedPayload)
      else
        Invalid(NonEmptyList.of(s"Valid signature count must be >= threshold ($threshold)"))
    }
  }
}
