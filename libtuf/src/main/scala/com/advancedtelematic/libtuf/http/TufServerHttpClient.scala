package com.advancedtelematic.libtuf.http

import com.advancedtelematic.libats.data.DataType.{Checksum, ValidChecksum}
import com.advancedtelematic.libats.data.ErrorRepresentation
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{DelegatedRoleName, RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.ErrorCodes
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{CompleteUploadRequest, ETag, GetSignedUrlResult, InitMultipartUploadResult, KeyId, MultipartUploadId, SignedPayload, TargetFilename, TufKeyPair, UploadPartETag}
import com.advancedtelematic.libtuf.http.CliHttpClient.{CliHttpBackend, CliHttpClientError, UnknownErrorRepr}
import com.advancedtelematic.libtuf.http.TufServerHttpClient._
import com.azure.storage.blob.specialized.BlockBlobClient
import com.azure.storage.blob.{BlobAsyncClient, BlobClientBuilder}
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import org.bouncycastle.util.encoders.Hex
import org.slf4j.LoggerFactory
import sttp.client._
import sttp.model.{Header, HeaderNames, StatusCode, Uri}

import java.io._
import java.net.{URI, URL}
import java.nio.file.Path
import java.security.MessageDigest
import java.util.{Base64, UUID}
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

object TufServerHttpClient {
  case class TargetsResponse(targets: SignedPayload[TargetsRole], checksum: Option[Refined[String, ValidChecksum]])

  case object RoleChecksumNotValid extends Exception("could not overwrite targets, trying to update an older version of role. Did you run `targets pull` ?") with NoStackTrace

  case class RoleNotFound(msg: String) extends Exception(s"role not found: $msg") with NoStackTrace

  case class UploadTargetTooBig(msg: String) extends Exception(msg) with NoStackTrace

  case class BinaryFileIntegrityCheckFailed(msg: String) extends Exception(msg) with NoStackTrace

  case class UriIsNotSupported(uri: Uri) extends Exception(s"redirect URI: $uri is not supported") with NoStackTrace

  case class TargetUploadFailed(uri: Uri) extends Exception(s"target upload failed for URI: $uri") with NoStackTrace

  case class UnexpectedResponse(msg: String) extends Exception(msg) with NoStackTrace
}

trait TufServerClient {
  def root(version: Option[Int] = None): Future[SignedPayload[RootRole]]

  def fetchKeyPair(keyId: KeyId): Future[TufKeyPair]

  def deleteKey(keyId: KeyId): Future[Unit]

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit]
}

trait ReposerverClient extends TufServerClient {
  def pushDelegation(name: DelegatedRoleName, delegation: SignedPayload[TargetsRole]): Future[Unit]

  def pullDelegation(name: DelegatedRoleName): Future[SignedPayload[TargetsRole]]

  def targets(): Future[TargetsResponse]

  def pushTargets(role: SignedPayload[TargetsRole], previousChecksum: Option[Refined[String, ValidChecksum]]): Future[Unit]

  def uploadTarget(targetFilename: TargetFilename, inputPath: Path, timeout: Duration, force: Boolean = false): Future[Unit]

  def verifyUploadedBinary(targetFilename: TargetFilename, localFileChecksum: Checksum): Future[Unit]
}

trait DirectorClient extends TufServerClient

abstract class TufServerHttpClient(uri: URI, httpBackend: CliHttpBackend)
                                  (implicit ec: ExecutionContext) extends CliHttpClient(httpBackend) {

  protected def uriPath: String

  protected def apiUri(path: String): Uri =
    Uri.apply(URI.create(uri.toString + uriPath + path))

  def root(version: Option[Int] = None): Future[SignedPayload[RootRole]] = {
    val filename = version match {
      case Some(v) => v + ".root.json"
      case None => "root.json"
    }

    val req = http.get(apiUri(filename))

    execHttp[SignedPayload[RootRole]](req) {
      case (404, error) => Future.failed(RoleNotFound(error.description))
    }.map(_.body)
  }

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit] = {
    val req = http.post(apiUri("root"))
    execJsonHttp[Unit, SignedPayload[RootRole]](req, signedRoot)()
  }

  def fetchKeyPair(keyId: KeyId): Future[TufKeyPair]

  def deleteKey(keyId: KeyId): Future[Unit]
}

class ReposerverHttpClient(uri: URI, httpBackend: CliHttpBackend)(implicit ec: ExecutionContext)
  extends TufServerHttpClient(uri, httpBackend) with ReposerverClient {

  private val _log = LoggerFactory.getLogger(this.getClass)

  protected def uriPath: String = "/api/v1/user_repo/"

  def fetchKeyPair(keyId: KeyId): Future[TufKeyPair] = {
    val req = http.get(apiUri("root/private_keys/" + keyId.value))
    execHttp[TufKeyPair](req)().map(_.body)
  }

  def deleteKey(keyId: KeyId): Future[Unit] = {
    val req = http.delete(apiUri("root/private_keys/" + keyId.value))
    execHttp[Unit](req)().map(_.body)
  }

  def targets(): Future[TargetsResponse] = {
    val req = http.get(apiUri("targets.json"))
    execHttp[SignedPayload[TargetsRole]](req)().map { response =>
      val checksumO = response.header("x-ats-role-checksum").flatMap { v => refineV[ValidChecksum](v).toOption }
      TargetsResponse(response.body, checksumO)
    }
  }

  def pushTargets(role: SignedPayload[TargetsRole], previousChecksum: Option[Refined[String, ValidChecksum]]): Future[Unit] = {
    val put = http.put(apiUri("targets"))
    val req = previousChecksum.map(e => put.header("x-ats-role-checksum", e.value)).getOrElse(put)

    execJsonHttp[Unit, SignedPayload[TargetsRole]](req, role) {
      case (412, errorRepr) if errorRepr.code.code == "role_checksum_mismatch" =>
        Future.failed(RoleChecksumNotValid)
      case (428, _) =>
        Future.failed(RoleChecksumNotValid)
    }
  }

  override def pushDelegation(name: DelegatedRoleName, delegation: SignedPayload[TargetsRole]): Future[Unit] = {
    val req = http.put(apiUri(s"delegations/${name.value}.json"))
    execJsonHttp[Unit, SignedPayload[TargetsRole]](req, delegation)()
  }

  override def pullDelegation(name: DelegatedRoleName): Future[SignedPayload[TargetsRole]] = {
    val req = http.get(apiUri(s"delegations/${name.value}.json"))
    execHttp[SignedPayload[TargetsRole]](req)().map(_.body)
  }

  override def uploadTarget(targetFilename: TargetFilename, inputPath: Path, timeout: Duration, force: Boolean = false): Future[Unit] = {
    val multipartUploadResult = for {
      inputFile <- Future.fromTry(Try(inputPath.toFile))
      initResult <- initMultipartUpload(targetFilename, inputFile.length(), force)
      result <- s3MultipartUpload(targetFilename, inputFile, initResult.uploadId, initResult.partSize, timeout)
    } yield result

    multipartUploadResult.recoverWith {
      case e: CliHttpClientError if e.remoteError.code == ErrorCodes.Reposerver.NotImplemented =>
        //Multipart upload is not supported for Azure Blob Storage.
        azureMultipartUpload(targetFilename, inputPath, timeout, force)
    }
  }

  def verifyUploadedBinary(targetFilename: TargetFilename, localFileChecksum: Checksum): Future[Unit] = {

    val req = http
      .get(apiUri(s"uploads/" + targetFilename.value))
      .followRedirects(false)

    _log.info(s"Verifying uploaded file integrity. It could take a few minutes. Please wait.")

    defaultHttpClient(req).flatMap {
      case rs if rs.isRedirect =>
        rs.header("Location") match {
          case Some(signedUrl) =>
            shaDigestFromUrl(new URL(signedUrl)) match {
              case Success(serverFileChecksum) if serverFileChecksum == localFileChecksum.hash.value =>
                _log.info(s"Integrity verification successful")
                Future.successful(())

              case Success(serverFileChecksum) if serverFileChecksum != localFileChecksum.hash.value =>
                val msg = s"Local file checksum (${localFileChecksum.hash}) not equal uploaded file checksum ($serverFileChecksum)"
                _log.error(s"Integrity verification failed. $msg")
                Future.failed(BinaryFileIntegrityCheckFailed(msg))

              case Failure(_: FileNotFoundException) =>
                val msg = s"File $targetFilename not found in server storage. Please upload file"
                _log.error(s"Integrity verification failed. $msg")
                Future.failed(BinaryFileIntegrityCheckFailed(msg))

              case Failure(exception) =>
                _log.error(s"Integrity verification failed. ${exception.getMessage}")
                Future.failed(BinaryFileIntegrityCheckFailed(exception.getMessage))
            }

          case None =>
            val msg = s"Cannot retrieve URL for download binary file"
            _log.error(s"Integrity verification failed. $msg")
            Future.failed(BinaryFileIntegrityCheckFailed(msg))
        }

      case rs =>
        val msg = s"Unexpected response from server: Status: ${rs.code}"
        _log.error(s"Integrity verification failed. $msg")
        Future.failed(BinaryFileIntegrityCheckFailed(msg))
    }
  }

  private def shaDigestFromUrl(url: URL): Try[String] = {
    val bufferSize = 1024 * 32
    val byteBuffer: Array[Byte] = new Array[Byte](bufferSize)

    @tailrec
    def process(inputStream: InputStream, processedBytes: Long = 0, shaDigest: MessageDigest = MessageDigest.getInstance("SHA-256")): String = {
      val readBytesCount: Int = inputStream.read(byteBuffer, 0, bufferSize)

      if (readBytesCount == -1) {
        inputStream.close()
        println()
        Hex.toHexString(shaDigest.digest())
      } else {
        val totalProcessedBytes = processedBytes + readBytesCount
        printRewriteLine(s"Processed: ${toMb(totalProcessedBytes)}Mb")
        shaDigest.update(byteBuffer.take(readBytesCount))
        process(inputStream, totalProcessedBytes, shaDigest)
      }
    }

    Try {
      val is = new BufferedInputStream(url.openStream())
      process(is)
    }
  }

  private def getAzureUploadUri(targetFilename: TargetFilename,
                                timeout: Duration,
                                force: Boolean): Future[Uri] = {
    val req = basicRequest.put(apiUri(s"uploads/" + targetFilename.value).params("force" -> force.toString))
      .readTimeout(timeout)
      .followRedirects(false)
      .response(asByteArrayAlways)

    httpBackend.send(req).flatMap {
      case r @ Response(_, StatusCode.Found, _, _, _) =>
        r.header("Location").fold[Either[String, Uri]](Left("No 'Location' header found."))(x => Uri.parse(x)) match {
          case Left(err) =>
            Future.failed(new Throwable(err))

          case Right(uri) if uri.host.endsWith("core.windows.net")  =>
            Future.successful(uri)

          case Right(uri) =>
            Future.failed(UriIsNotSupported(uri))
        }

      case resp =>
        handleResponse[Unit](req, resp) {
          case (StatusCode.PayloadTooLarge.code, err) if err.code == ErrorCodes.Reposerver.PayloadTooLarge =>
            _log.debug(s"Error from server: $err")
            Future.failed(UploadTargetTooBig(err.description))
          case (statusCode, errRepr) =>
            Future.failed(CliHttpClientError(s"Bad StatusCode $statusCode during redirect URI fetch", errRepr))
        }.flatMap(_ => Future.failed(UnexpectedResponse(s"Unexpected response during redirect URI fetch")))
    }
  }


  private def azureMultipartUpload(targetFilename: TargetFilename, inputPath: Path, timeout: Duration, force: Boolean): Future[Unit] = {
    val partSize = BlobAsyncClient.BLOB_DEFAULT_UPLOAD_BLOCK_SIZE

    val inputFile = inputPath.toFile
    val totalSize = inputFile.length()
    val inputStream = new FileInputStream(inputFile)

    def processChunk(client: BlockBlobClient, eTags: Seq[String] = Seq.empty, bytesProcessed: Long = 0): Future[Seq[String]] = {

      val available = inputStream.available()
      val bufferSize = Math.min(partSize, available)
      val eTag = Base64.getEncoder.encodeToString(UUID.randomUUID().toString.getBytes())
      val byteBuffer = new Array[Byte](bufferSize)
      inputStream.read(byteBuffer)
      val subStream = new ByteArrayInputStream(byteBuffer)

      if (bufferSize == 0) {
        inputStream.close()
        Future.successful(eTags)
      } else {
        Future(client.stageBlock(eTag, subStream, bufferSize)).flatMap { _ =>
          printProgress(totalSize, bytesProcessed + bufferSize)
          processChunk(client, eTags :+ eTag, bytesProcessed + bufferSize)
        }
      }
    }

    _log.info("Uploading file, this may take a while")
    for {
      uploadUri <- getAzureUploadUri(targetFilename, timeout, force)
      client = new BlobClientBuilder().endpoint(uploadUri.toString()).buildClient().getBlockBlobClient
      tags <- processChunk(client)
      _ <- Future(client.commitBlockList(tags.asJava))
    } yield {
      _log.info(s"Upload completed")
    }
  }

  private def s3MultipartUpload(targetFilename: TargetFilename, inputFile: File, uploadId: MultipartUploadId, partSize: Long, timeout: Duration): Future[Unit] = {

    val totalSize = inputFile.length()
    val inputStream: FileInputStream = new FileInputStream(inputFile)

    def processChunk(inputStream: FileInputStream, part: Int = 1, bytesProcessed: Long = 0, eTags: Seq[UploadPartETag] = Seq.empty): Future[Seq[UploadPartETag]] = {

      val available = inputStream.available()
      val bufferSize = Math.min(partSize, available).toInt
      val byteBuffer = new Array[Byte](bufferSize)
      inputStream.read(byteBuffer)

      if (bufferSize == 0) {
        inputStream.close()
        println()
        Future.successful(eTags)
      } else {
        getSignedUrlAndUploadPart(targetFilename, uploadId, part, byteBuffer, timeout)
          .flatMap { tag =>
            printProgress(totalSize, bytesProcessed + bufferSize)
            processChunk(inputStream, part + 1, bytesProcessed + bufferSize, tag +: eTags)
          }
      }
    }

    for {
      tags <- processChunk(inputStream)
      _ <- completeMultipartUpload(targetFilename, uploadId, tags)
    } yield {
      _log.info(s"Upload completed")
    }
  }

  private def printProgress(total: Long, current: Long): Unit = {
    val str = s"Uploading: uploaded ${toMb(current)}Mb of ${toMb(total)}Mb"
    printRewriteLine(str)
  }

  private def toMb(value: Long) = BigDecimal(value.toDouble / 1024 / 1024).setScale(2, BigDecimal.RoundingMode.HALF_UP)

  private def printRewriteLine(msg: String): Unit = {
    print(s"\u001B[100D$msg")
  }

  private def initMultipartUpload(targetFilename: TargetFilename, fileSize: Long, force: Boolean): Future[InitMultipartUploadResult] = {
    val req = http
      .post(apiUri(s"multipart/initiate/" + targetFilename.value).params("fileSize" -> fileSize.toString, "force" -> force.toString))
      .followRedirects(false)

    execHttp[InitMultipartUploadResult](req)().map(_.body)
  }

  private def getSignedUrlAndUploadPart(targetFilename: TargetFilename, uploadId: MultipartUploadId, part: Int, data: Array[Byte], timeout: Duration): Future[UploadPartETag] = {
    val digest = MessageDigest.getInstance("MD5").digest(data)
    val md5Hash = Base64.getEncoder.encodeToString(digest)
    val contentLength = data.length

    for {
      url <- getSignedUrl(targetFilename, uploadId, part, md5Hash, contentLength)
      eTag <- uploadPart(url, part, data, md5Hash, timeout)
    } yield eTag
  }

  private def getSignedUrl(targetFilename: TargetFilename, uploadId: MultipartUploadId, part: Int, md5Hash: String, length: Int): Future[URI] = {
    val url = apiUri(s"multipart/url/${targetFilename.value}")
      .params(("part" -> part.toString),
        ("uploadId" -> uploadId.value),
        ("md5" -> md5Hash),
        ("contentLength" -> length.toString))

    val req = http.get(url).followRedirects(false)

    val rs = execHttp[GetSignedUrlResult](req, retryingHttpClient())().map(_.body)

    rs.onComplete {
      case Success(v) => _log.debug(s"Get signed URL response: $v")
      case Failure(e) => _log.error(s"Get signed URL error: ${e.getMessage}")
    }

    rs.map(_.uri)
  }

  private def uploadPart(uri: URI, part: Int, data: Array[Byte], md5Hash: String, timeout: Duration): Future[UploadPartETag] = {

    val req = http.put(Uri(uri))
      .body(data)
      .readTimeout(timeout)
      .header(HeaderNames.ContentMd5, md5Hash)
      .header(Header.contentLength(data.length))
      .followRedirects(false)

    retryingHttpClient()(req).flatMap { response =>
      response.header(HeaderNames.Etag) match {
        case Some(eTag) =>
          _log.debug("Uploaded part ETag: " + eTag)
          Future.successful(UploadPartETag(part, ETag(eTag)))
        case None =>
          Future.failed(new Exception("ETag not found in response headers"))
      }
    }
  }

  def completeMultipartUpload(targetFilename: TargetFilename, uploadId: MultipartUploadId, partETags: Seq[UploadPartETag]): Future[Unit] = {
    val req = http
      .put(apiUri(s"multipart/complete/" + targetFilename.value))
      .followRedirects(false)

    val rs = execJsonHttp[Unit, CompleteUploadRequest](req, CompleteUploadRequest(uploadId, partETags), retryingHttpClient())()

    rs.onComplete {
      case Success(v) => _log.debug(s"Complete multipart upload response: $v")
      case Failure(e) => _log.error(s"Complete multipart upload error: ${e.getMessage}")
    }

    rs.map(_ => ())
  }
}

class DirectorHttpClient(uri: URI, httpBackend: CliHttpBackend)
                        (implicit ec: ExecutionContext) extends TufServerHttpClient(uri, httpBackend) with DirectorClient {

  // assumes talking to the Director through the API gateway
  protected def uriPath: String = "/api/v1/director/admin/repo/"

  def fetchKeyPair(keyId: KeyId): Future[TufKeyPair] = {
    val req = http.get(apiUri("private_keys/" + keyId.value))
    execHttp[TufKeyPair](req)().map(_.body)
  }

  def deleteKey(keyId: KeyId): Future[Unit] = {
    val req = http.delete(apiUri("private_keys/" + keyId.value))
    execHttp[Unit](req)().map(_.body)
  }
}
