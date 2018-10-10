package com.advancedtelematic.tuf.reposerver.http

import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType._
import akka.http.scaladsl.util.FastFuture
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, TargetCustom, TargetsRole, TufRole}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, SignedPayload, TargetFilename}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{StorageMethod, TargetItem}
import com.advancedtelematic.tuf.reposerver.db.{SignedRoleRepositorySupport, TargetItemRepositorySupport}
import io.circe.Encoder
import cats.implicits._
import com.advancedtelematic.libats.data.DataType.{Checksum, Namespace}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.TufRole._

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.db.DbSignedRoleRepository.SignedRoleNotFound
import com.advancedtelematic.tuf.reposerver.http.RoleChecksumHeader.RoleChecksum

class OfflineSignedRoleStorage(keyserverClient: KeyserverClient)
                                        (implicit val db: Database, val ec: ExecutionContext)
  extends SignedRoleRepositorySupport with TargetItemRepositorySupport{

  private val signedRoleGeneration = new SignedRoleGeneration(keyserverClient)

  def store(repoId: RepoId, signedPayload: SignedPayload[TargetsRole]): Future[ValidatedNel[String, (Seq[TargetItem], SignedRole[TargetsRole])]] =
    for {
      validatedPayloadSig <- payloadSignatureIsValid(repoId, signedPayload)
      existingTargets <- targetItemRepo.findFor(repoId)
      targetItemsValidated = validatedPayloadSig.andThen(_ => validatedPayloadTargets(repoId, signedPayload, existingTargets))
      signedRoleValidated <- targetItemsValidated match {
        case Valid(items) =>
          val signedTargetRole = SignedRole.withChecksum[TargetsRole](repoId, signedPayload.asJsonSignedPayload, signedPayload.signed.version, signedPayload.signed.expires)

          signedRoleGeneration.regenerateSignedDependent(repoId, signedTargetRole, signedPayload.signed.expires)
            .flatMap { case (targets, timestamps) => signedRoleRepository.storeAll(targetItemRepo)(repoId: RepoId, List(signedTargetRole, targets, timestamps), items) }
            .map(_ => (existingTargets, signedTargetRole).validNel)
        case i @ Invalid(_) =>
          FastFuture.successful(i)
      }
    } yield signedRoleValidated

  private def validatedPayloadTargets(repoId: RepoId, payload: SignedPayload[TargetsRole], existingTargets: Seq[TargetItem]): ValidatedNel[String, List[TargetItem]] = {
    def errorMsg(filename: TargetFilename, msg: Any): String = s"target item error ${filename.value}: $msg"

    def validateNewChecksum(filename: TargetFilename, newItem: ClientTargetItem): Either[String, Checksum] = {
      newItem.hashes
        .headOption
        .map { case (method, hash) => Checksum(method, hash) }
        .toRight(errorMsg(filename, "Invalid/Missing Checksum"))
    }

    def validateExistingTarget(filename: TargetFilename, oldItem: TargetItem, newItem: ClientTargetItem): Either[String, TargetItem] =
      for {
        newTargetCustom <- newItem.custom match {
          case Some(customJson) => customJson.as[TargetCustom].leftMap(errorMsg(filename, _)).map(_.some)
          case None => Right(None)
        }
        checksum <- validateNewChecksum(filename, newItem)
      } yield TargetItem(repoId, filename, oldItem.uri, checksum, newItem.length, newTargetCustom, oldItem.storageMethod)

    def validateNewTarget(filename: TargetFilename, item: ClientTargetItem): Either[String, TargetItem] =
      for {
        json <- item.custom.toRight(errorMsg(filename, "new offline signed target items must contain custom metadata"))
        targetCustom <- json.as[TargetCustom].leftMap(errorMsg(filename, _))
        checksum <- validateNewChecksum(filename, item)
      } yield TargetItem(repoId, filename, targetCustom.uri.map(_.toUri), checksum, item.length, Some(targetCustom), storageMethod = StorageMethod.Unmanaged)

    val existingTargetsAsMap = existingTargets.map { ti => ti.filename -> ti }.toMap

    val newTargetItems = payload.signed.targets.map { case (filename, item) =>
      existingTargetsAsMap.get(filename) match {
        case Some(ti) => validateExistingTarget(filename, ti, item)
        case None => validateNewTarget(filename, item)
      }
    }

    newTargetItems.map(_.toValidatedNel).toList.sequence
  }

  private def payloadSignatureIsValid[T : TufRole : Encoder](repoId: RepoId, signedPayload: SignedPayload[T]): Future[ValidatedNel[String, SignedPayload[T]]] = async {
    val rootRole = await(keyserverClient.fetchRootRole(repoId)).signed

    TufCrypto.payloadSignatureIsValid(rootRole, signedPayload)
  }

  def saveTargetRole(namespace: Namespace, signedTargetPayload: SignedPayload[TargetsRole], repoId: RepoId, checksum: Option[RoleChecksum]): Future[ValidatedNel[String, (Seq[TargetItem], SignedRole[TargetsRole])]] =
    signedRoleRepository.find[TargetsRole](repoId).flatMap { existingRole =>
      updateSignedTarget(repoId, namespace, signedTargetPayload, existingRole, checksum)
    }.recoverWith {
      case SignedRoleNotFound =>
        store(repoId, signedTargetPayload)
    }

  private def updateSignedTarget(repoId: RepoId, namespace: Namespace, signedTargetPayload: SignedPayload[TargetsRole],
                                 existingRole: SignedRole[TargetsRole], checksumOpt: Option[RoleChecksum]) =
    checksumOpt match {
      case Some(checksum) if existingRole.checksum.hash == checksum =>
        store(repoId, signedTargetPayload)
      case Some(_) =>
        Future.failed(Errors.RoleChecksumMismatch) // TODO: Should be 412?
      case None =>
        Future.failed(Errors.RoleChecksumNotProvided)
    }
}
