package com.advancedtelematic.tuf.reposerver.target_store

import java.time.Instant

import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.model._
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.advancedtelematic.libats.messaging.MessageBusPublisher
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.Messages.{PackageStorageUsage, TufTargetAdded}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.TargetItem
import com.advancedtelematic.tuf.reposerver.db.TargetItemRepositorySupport
import com.advancedtelematic.tuf.reposerver.http.SignedRoleGeneration
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore.{TargetBytes, TargetRedirect}
import org.slf4j.LoggerFactory
import slick.jdbc.MySQLProfile.api.Database

import scala.concurrent.{ExecutionContext, Future}

class TargetUpload(roleKeyStore: KeyserverClient,
                   targetStore: TargetStore,
                   messageBusPublisher: MessageBusPublisher)
                  (implicit val db: Database, val ec: ExecutionContext)
  extends TargetItemRepositorySupport {

  private val signedRoleGeneration = new SignedRoleGeneration(roleKeyStore)

  private val _log = LoggerFactory.getLogger(this.getClass)

  def publishUploadMessages(repoId: RepoId, target: TargetItem): Future[Unit] = {
    val f = for {
      (namespace, usage) <- targetItemRepo.usage(repoId)
      _ <- messageBusPublisher.publish(PackageStorageUsage(namespace.get, Instant.now, usage))
      _ <- messageBusPublisher.publish(
        TufTargetAdded(namespace.get, target.filename, target.checksum, target.length, target.custom))
    } yield ()

    f.recover {
      case ex =>
        _log.warn("Could not publish bus message", ex)
    }
  }


  def store(repoId: RepoId, targetFile: TargetFilename, fileData: Source[ByteString, Any], custom: TargetCustom): Future[Unit] = {
    for {
      storeResult <- targetStore.store(repoId, targetFile, fileData)
      item = TargetItem(repoId, targetFile, storeResult.uri, storeResult.checksum, storeResult.size, Some(custom))
      _ <- signedRoleGeneration.addToTarget(item)
      _ <- publishUploadMessages(repoId, item)
    } yield ()
  }

  def retrieve(repoId: RepoId, targetFilename: TargetFilename): Future[HttpResponse] = {
    // TODO: Publish usage/inflight https://advancedtelematic.atlassian.net/browse/PRO-2803
    targetStore.retrieve(repoId, targetFilename).map {
      case TargetBytes(bytes, size) =>
        val entity = HttpEntity(MediaTypes.`application/octet-stream`, size, bytes)
        HttpResponse(StatusCodes.OK, entity = entity)

      case TargetRedirect(uri) =>
        HttpResponse(StatusCodes.Found, List(Location(uri)))
    }
  }
}