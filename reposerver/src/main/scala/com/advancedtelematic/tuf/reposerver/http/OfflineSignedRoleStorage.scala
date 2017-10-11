package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.util.FastFuture
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import com.advancedtelematic.libtuf.data.ClientDataType.{TargetCustom, TargetsRole, VersionedRole}
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RepoId, RoleType, SignedPayload, TargetFilename}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, StorageMethod, TargetItem}
import com.advancedtelematic.tuf.reposerver.db.{SignedRoleRepositorySupport, TargetItemRepositorySupport}
import io.circe.Encoder
import cats.implicits._
import com.advancedtelematic.libats.data.DataType.Checksum
import com.advancedtelematic.libtuf.data.ClientCodecs._
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libats.http.HttpCodecs._

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.libtuf.crypt.SignedPayloadSignatureOps._
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient

class OfflineSignedRoleStorage(keyserverClient: KeyserverClient)
                                        (implicit val db: Database, val ec: ExecutionContext)
  extends SignedRoleRepositorySupport with TargetItemRepositorySupport{

  private val signedRoleGeneration = new SignedRoleGeneration(keyserverClient)

  def store(repoId: RepoId, signedPayload: SignedPayload[TargetsRole]): Future[ValidatedNel[String, SignedRole]] =
    payloadSignatureIsValid(repoId, signedPayload).map { validated =>
      validated.product(payloadTargets(repoId, signedPayload))
    }.flatMap {
      case Valid((_, items)) =>
        val signedTargetRole = SignedRole.withChecksum(repoId, RoleType.TARGETS, signedPayload, signedPayload.signed.version, signedPayload.signed.expires)
        signedRoleGeneration.regenerateSignedDependent(repoId, signedTargetRole, signedPayload.signed.expires)
          .flatMap(dependent => signedRoleRepo.storeAll(targetItemRepo)(signedTargetRole :: dependent, items))
          .map(_ => signedTargetRole.validNel)
      case i @ Invalid(_) =>
        FastFuture.successful(i)
    }

  private def payloadTargets(repoId: RepoId, payload: SignedPayload[TargetsRole]): ValidatedNel[String, List[TargetItem]] = {
    def errorMsg(filename: TargetFilename, msg: Any): String = s"target item error ${filename.value}: $msg"

    val items = payload.signed.targets.map { case (filename, item) =>
      for {
        json <- item.custom.toRight(errorMsg(filename, "All offline signed target items must contain custom metadata"))
        uri <- json.hcursor.downField("uri").as[Uri].leftMap(errorMsg(filename, _))
        targetCustom <- json.as[TargetCustom].leftMap(errorMsg(filename, _))
        targetItem <- {
          item.hashes
            .headOption
            .map { case (method, hash) => Checksum(method, hash) }
            .map { checksum => TargetItem(repoId, filename, uri, checksum, item.length, Some(targetCustom), storageMethod = StorageMethod.Unmanaged) }
            .toRight(errorMsg(filename, "Invalid/Missing Checksum"))
        }
      } yield targetItem
    }

    items.map(_.toValidatedNel).toList.sequenceU
  }

  private def payloadSignatureIsValid[T <: VersionedRole : Encoder](repoId: RepoId, signedPayload: SignedPayload[T]): Future[ValidatedNel[String, SignedPayload[T]]] = async {
    val rootRole = await(keyserverClient.fetchRootRole(repoId)).signed

    val publicKeys = rootRole.keys.filterKeys(keyId => rootRole.roles(signedPayload.signed.roleType).keyids.contains(keyId))

    val sigsByKeyId = signedPayload.signatures.map(s => s.keyid -> s).toMap

    val validSignatureCount: ValidatedNel[String, List[KeyId]] =
      sigsByKeyId.par.map { case (keyId, sig) =>
        publicKeys.get(keyId)
          .toRight(s"No public key available for key ${sig.keyid}")
          .ensure(s"Invalid signature for key ${sig.keyid}") { key =>
            signedPayload.isValidFor(key)
          }
          .map(_.id)
          .toValidatedNel
      }.toList.sequenceU

    val threshold = rootRole.roles(RoleType.TARGETS).threshold

    validSignatureCount.ensure(NonEmptyList.of(s"Valid signature count must be >= threshold ($threshold)")) { validSignatures =>
      validSignatures.size >= threshold && threshold > 0
    }.map(_ => signedPayload)
  }
}
