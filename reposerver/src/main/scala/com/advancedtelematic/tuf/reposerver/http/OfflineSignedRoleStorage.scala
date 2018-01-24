package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.util.FastFuture
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, TargetCustom, TargetsRole, TufRole}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, SignedPayload, TargetFilename}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, StorageMethod, TargetItem}
import com.advancedtelematic.tuf.reposerver.db.{SignedRoleRepositorySupport, TargetItemRepositorySupport}
import io.circe.Encoder
import cats.implicits._
import com.advancedtelematic.libats.data.DataType.Checksum
import com.advancedtelematic.libtuf.data.ClientCodecs._
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libats.http.HttpCodecs._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.TufRole._

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient

class OfflineSignedRoleStorage(keyserverClient: KeyserverClient)
                                        (implicit val db: Database, val ec: ExecutionContext)
  extends SignedRoleRepositorySupport with TargetItemRepositorySupport{

  private val signedRoleGeneration = new SignedRoleGeneration(keyserverClient)

  def store(repoId: RepoId, signedPayload: SignedPayload[TargetsRole]): Future[ValidatedNel[String, SignedRole]] =
    for {
      validatedPayloadSig <- payloadSignatureIsValid(repoId, signedPayload)
      existingTargets <- targetItemRepo.findFor(repoId)
      targetItemsValidated = validatedPayloadSig.andThen(_ => validatedPayloadTargets(repoId, signedPayload, existingTargets))
      signedRoleValidated <- targetItemsValidated match {
        case Valid(items) =>
          val signedTargetRole = SignedRole.withChecksum(repoId, RoleType.TARGETS, signedPayload, signedPayload.signed.version, signedPayload.signed.expires)

          signedRoleGeneration.regenerateSignedDependent(repoId, signedTargetRole, signedPayload.signed.expires)
            .flatMap(dependent => signedRoleRepo.storeAll(targetItemRepo)(repoId: RepoId, signedTargetRole :: dependent, items))
            .map(_ => signedTargetRole.validNel)
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
        uri <- json.hcursor.downField("uri").as[Uri].leftMap(errorMsg(filename, _))
        targetCustom <- json.as[TargetCustom].leftMap(errorMsg(filename, _))
        storageMethod = existingTargets.find(_.filename ==  filename).map(_.storageMethod).getOrElse(StorageMethod.Unmanaged)
        checksum <- validateNewChecksum(filename, item)
      } yield TargetItem(repoId, filename, uri, checksum, item.length, Some(targetCustom), storageMethod = storageMethod)

    val existingTargetsAsMap = existingTargets.map { ti => ti.filename -> ti }.toMap

    val newTargetItems = payload.signed.targets.map { case (filename, item) =>
      existingTargetsAsMap.get(filename) match {
        case Some(ti) => validateExistingTarget(filename, ti, item)
        case None => validateNewTarget(filename, item)
      }
    }

    newTargetItems.map(_.toValidatedNel).toList.sequenceU
  }

  private def payloadSignatureIsValid[T : TufRole : Encoder](repoId: RepoId, signedPayload: SignedPayload[T]): Future[ValidatedNel[String, SignedPayload[T]]] = async {
    val rootRole = await(keyserverClient.fetchRootRole(repoId)).signed

    TufCrypto.payloadSignatureIsValid(rootRole, signedPayload)
  }
}
