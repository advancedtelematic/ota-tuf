package com.advancedtelematic.tuf.reposerver.target_store

import java.time.Instant

import cats.implicits._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.advancedtelematic.libats.messaging.MessageBusPublisher
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.Messages.PackageStorageUsage
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.TargetItem
import com.advancedtelematic.tuf.reposerver.db.TargetItemRepositorySupport
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore.{TargetBytes, TargetRedirect}
import org.slf4j.LoggerFactory
import slick.jdbc.MySQLProfile.api.Database
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.StorageMethod._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace

object TargetUpload {
  def apply(roleKeyStore: KeyserverClient,
            targetStore: TargetStore,
            messageBusPublisher: MessageBusPublisher)
           (implicit db: Database, ec: ExecutionContext, system: ActorSystem, mat: Materializer): TargetUpload = {
    val _http = Http()
    new TargetUpload(roleKeyStore, targetStore, req => _http.singleRequest(req), messageBusPublisher)
  }
}

class TargetUpload(roleKeyStore: KeyserverClient,
                   targetStore: TargetStore,
                   httpClient: HttpRequest => Future[HttpResponse],
                   messageBusPublisher: MessageBusPublisher)
                  (implicit val db: Database, val ec: ExecutionContext)
  extends TargetItemRepositorySupport {

  private val _log = LoggerFactory.getLogger(this.getClass)

  case class DownloadError(msg: String) extends Exception(msg) with NoStackTrace

  def publishUploadMessages(repoId: RepoId): Future[Unit] = {
    val f = for {
      (namespace, usage) <- targetItemRepo.usage(repoId)
      _ <- messageBusPublisher.publish(PackageStorageUsage(namespace.get, Instant.now, usage))
    } yield ()

    f.handleError { ex =>
      _log.warn("Could not publish bus message", ex)
    }
  }

  def store(repoId: RepoId, targetFile: TargetFilename, fileData: Source[ByteString, Any], custom: TargetCustom): Future[TargetItem] = {
    for {
      storeResult <- targetStore.store(repoId, targetFile, fileData)
      _ <- publishUploadMessages(repoId)
    } yield TargetItem(repoId, targetFile, storeResult.uri, storeResult.checksum, storeResult.size, Some(custom))
  }

  def storeFromUri(repoId: RepoId, targetFile: TargetFilename, fileUri: Uri, custom: TargetCustom): Future[TargetItem] = {
    httpClient(HttpRequest(uri = fileUri)).flatMap { response =>
      response.status match {
        case StatusCodes.OK => store(repoId, targetFile, response.entity.dataBytes, custom)
        case _ => Future.failed(DownloadError("Unable to download file " + fileUri.toString))
      }
    }
  }

  def retrieve(repoId: RepoId, targetFilename: TargetFilename): Future[HttpResponse] = {
    targetItemRepo.findByFilename(repoId, targetFilename).flatMap { item =>
      item.storageMethod match {
        case Managed =>
          retrieveFromManaged(repoId, targetFilename)
        case Unmanaged =>
          redirectToUnmanaged(item)
      }
    }
  }

  private def redirectToUnmanaged(item: TargetItem): Future[HttpResponse] = FastFuture.successful {
    HttpResponse(StatusCodes.Found, List(Location(item.uri)))
  }

  private def retrieveFromManaged(repoId: RepoId, targetFilename: TargetFilename): Future[HttpResponse] = {
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
