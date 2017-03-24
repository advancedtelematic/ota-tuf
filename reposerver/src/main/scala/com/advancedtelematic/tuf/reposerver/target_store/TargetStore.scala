package com.advancedtelematic.tuf.reposerver.target_store

import java.io.File
import java.nio.file.{Files, Path, Paths}

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, StatusCodes, Uri}
import akka.http.scaladsl.util.FastFuture
import akka.stream.{ActorMaterializer, IOResult, Materializer}
import akka.stream.scaladsl.{FileIO, Keep, Sink, Source}
import akka.util.ByteString
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf.crypt.Sha256Digest
import com.advancedtelematic.libtuf.data.ClientDataType.TargetFilename
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, RepoId}
import com.advancedtelematic.tuf.reposerver.http.{ErrorCodes, Errors}
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore.{TargetBytes, TargetRetrieveResult, TargetStoreResult}
import com.amazonaws.auth.AWSCredentials
import com.typesafe.config.Config
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


object TargetStore {
  case class TargetStoreResult(uri: Uri, checksum: Checksum, size: Long)

  sealed trait TargetRetrieveResult
  case class TargetRedirect(uri: Uri) extends TargetRetrieveResult
  case class TargetBytes(bytes: Source[ByteString, Future[Done]], size: Long) extends TargetRetrieveResult
}


trait TargetStore {
  private val _log = LoggerFactory.getLogger(this.getClass)

  def store(repoId: RepoId, filename: TargetFilename, fileData: Source[ByteString, Any]): Future[TargetStoreResult]

  def retrieve(repoId: RepoId, filename: TargetFilename): Future[TargetRetrieveResult]

  protected def write(fileData: Source[ByteString, Any],
                      writeSink: Sink[ByteString, Future[(Uri, Long)]])
                     (implicit system: ActorSystem, mat: Materializer): Future[TargetStoreResult] = {
    implicit val ec = system.dispatcher
    val digestCalculator = Sha256Digest.asSink

    val (digestF, resultF) = fileData
      .alsoToMat(digestCalculator)(Keep.right)
      .toMat(writeSink)(Keep.both)
      .run()

    val writeAsync = for {
      digest <- digestF
      (uri, sizeBytes) <- resultF
    } yield TargetStoreResult(uri, digest, sizeBytes)

    writeAsync.andThen(logResult)
  }

  private val logResult: PartialFunction[Try[TargetStoreResult], Unit] = {
    case Success(packageStoreResult) =>
      _log.debug(s"Package $packageStoreResult uploaded")
    case Failure(t) =>
      _log.error(s"Failed to write package", t)
  }

  protected def storageFilename(repoId: RepoId, targetFilename: TargetFilename): Path = {
    val prefixHash = Sha256Digest.digest(repoId.uuid.toString.getBytes).hash.get

    val fileNameHash = Sha256Digest.digest(targetFilename.get.getBytes).hash.get

    val (prefix, dir) = fileNameHash.splitAt(2)

    Paths.get(prefixHash, prefix, dir)
  }
}

object LocalTargetStore {
  private val _log = LoggerFactory.getLogger(this.getClass)

  def apply(root: String)(implicit system: ActorSystem, mat: Materializer): LocalTargetStore = {
    val f = new File(root)
    if(!f.exists() && !f.getParentFile.canWrite) {
      throw new IllegalArgumentException(s"Could not open $root as local target store")
    } else if (!f.exists()) {
      Files.createDirectory(f.toPath)
      _log.info(s"Created local fs blob store directory: $root")
    }

    _log.info(s"local fs blob store set to $root")
    new LocalTargetStore(f)
  }
}

class LocalTargetStore(root: File)(implicit val system: ActorSystem, val mat: Materializer) extends TargetStore {
  import system.dispatcher

  val log = LoggerFactory.getLogger(this.getClass)

  override def store(repoId: RepoId, filename: TargetFilename, fileData: Source[ByteString, Any]): Future[TargetStoreResult] = {
    val sink = localFileSink(repoId, filename, fileData)
    write(fileData, sink)
  }

  override def retrieve(repoId: RepoId, filename: TargetFilename): Future[TargetRetrieveResult] = {
    val storePath = root.toPath.resolve(storageFilename(repoId, filename))

    if(!storePath.toFile.canRead)
      Future.failed(Errors.TargetNotFoundError)
    else {
      val size = storePath.toFile.length()

      val source = FileIO.fromPath(storePath).mapMaterializedValue {
        _.flatMap { ioResult =>
          if (ioResult.wasSuccessful)
            FastFuture.successful(Done)
          else
            FastFuture.failed(ioResult.getError)
        }
      }

      Future.successful(TargetBytes(source, size))
    }
  }

  protected def localFileSink(repoId: RepoId,
                              filename: TargetFilename,
                              fileData: Source[ByteString, Any]): Sink[ByteString, Future[(Uri, Long)]] = {
    val storePath = root.toPath.resolve(storageFilename(repoId, filename))

    Files.createDirectories(storePath.getParent)

    val uri = Uri(storePath.toAbsolutePath.toString)

    FileIO.toPath(storePath).mapMaterializedValue {
      _.flatMap { result =>
        if(result.wasSuccessful)
          Future.successful((uri, result.count))
        else
          Future.failed(result.getError)
      }
    }
  }
}
