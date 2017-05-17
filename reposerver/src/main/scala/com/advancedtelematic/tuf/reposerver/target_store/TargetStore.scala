package com.advancedtelematic.tuf.reposerver.target_store

import java.nio.file.{Path, Paths}

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.stream.Materializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.util.ByteString
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libtuf.crypt.Sha256Digest
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, RepoId}
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore.{TargetRetrieveResult, TargetStoreResult}
import org.slf4j.LoggerFactory

import scala.concurrent.Future
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
    val prefixHash = Sha256Digest.digest(repoId.uuid.toString.getBytes).hash.value

    val fileNameHash = Sha256Digest.digest(targetFilename.value.getBytes).hash.value

    val (prefix, dir) = fileNameHash.splitAt(2)

    Paths.get(prefixHash, prefix, dir)
  }
}



