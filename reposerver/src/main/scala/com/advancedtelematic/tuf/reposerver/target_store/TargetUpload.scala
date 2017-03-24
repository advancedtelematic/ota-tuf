package com.advancedtelematic.tuf.reposerver.target_store

import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.model._
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.advancedtelematic.libats.messaging.MessageBusPublisher
import com.advancedtelematic.libtuf.data.ClientDataType.{TargetCustom, TargetFilename}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.TargetItem
import com.advancedtelematic.tuf.reposerver.db.TargetItemRepositorySupport
import com.advancedtelematic.tuf.reposerver.http.SignedRoleGeneration
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore.{TargetBytes, TargetRedirect}
import slick.driver.MySQLDriver.api.Database

import scala.concurrent.{ExecutionContext, Future}

class TargetUpload(roleKeyStore: KeyserverClient,
                   targetStore: TargetStore,
                   messageBusPublisher: MessageBusPublisher)
                  (implicit val db: Database, val ec: ExecutionContext)
  extends TargetItemRepositorySupport {

  private val signedRoleGeneration = new SignedRoleGeneration(roleKeyStore)

  def publishUploadMessages(repoId: RepoId): Future[Unit] = for {
    usage <- targetItemRepo.usage(repoId)
    // bus.publishSafe()
    // publish new usage
  } yield ()

  def store(repoId: RepoId, targetFile: TargetFilename, fileData: Source[ByteString, Any], custom: TargetCustom): Future[Unit] = {
    for {
      storeResult <- targetStore.store(repoId, targetFile, fileData)
      item = TargetItem(repoId, targetFile, storeResult.uri, storeResult.checksum, storeResult.size, Some(custom))
            _ <- signedRoleGeneration.addToTarget(item)
      _ <- publishUploadMessages(repoId)
    } yield ()
  }

  def retrieve(repoId: RepoId, targetFilename: TargetFilename): Future[HttpResponse] = {
    // TODO: Publish usage/inflight
    targetStore.retrieve(repoId, targetFilename).map {
      case TargetBytes(bytes, size) =>
        val entity = HttpEntity(MediaTypes.`application/octet-stream`, size, bytes)
        HttpResponse(StatusCodes.OK, entity = entity)

      case TargetRedirect(uri) =>
        HttpResponse(StatusCodes.Found, List(Location(uri)))
    }
  }
}
