package com.advancedtelematic.libtuf_server.repo.client

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.Uri.Path.Slash
import akka.http.scaladsl.model.Uri.{Path, Query}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.advancedtelematic.libats.data.DataType.{Checksum, Namespace}
import com.advancedtelematic.libats.data.ErrorCode
import com.advancedtelematic.libats.http.Errors.{RawError, RemoteServiceError}
import com.advancedtelematic.libats.http.ServiceHttpClientSupport
import com.advancedtelematic.libats.http.tracing.Tracing.RequestTracing
import com.advancedtelematic.libats.http.tracing.TracingHttpClient
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, KeyType, RepoId, SignedPayload, TargetName, TargetVersion}
import com.advancedtelematic.libtuf_server.data.Requests.CreateRepositoryRequest
import com.advancedtelematic.libtuf_server.repo.client.ReposerverClient.{KeysNotReady, NotFound, RootNotInKeyserver}
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder, Json}
import com.advancedtelematic.libats.codecs.CirceCodecs._
import com.advancedtelematic.libtuf_server.repo.server.DataType._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.http.HttpCodecs._

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.{Failure, Success}
import com.advancedtelematic.libtuf.data.TufCodecs._

object ReposerverClient {

  object RequestTargetItem {
    implicit val encoder: Encoder[RequestTargetItem] = deriveEncoder
    implicit val decoder: Decoder[RequestTargetItem] = deriveDecoder
  }

  case class RequestTargetItem2(uri: Uri, checksum: Checksum,
                                targetFormat: Option[TargetFormat],
                                name: Option[TargetName],
                                version: Option[TargetVersion],
                                hardwareIds: Seq[HardwareIdentifier],
                                length: Long)

  case class RequestTargetItem(uri: Uri, checksum: Checksum,
                               targetFormat: Option[TargetFormat],
                               name: Option[TargetName],
                               version: Option[TargetVersion],
                               hardwareIds: Seq[HardwareIdentifier],
                               length: Long)

  val KeysNotReady = RawError(ErrorCode("keys_not_ready"), StatusCodes.Locked, "keys not ready")
  val RootNotInKeyserver = RawError(ErrorCode("root_role_not_in_keyserver"), StatusCodes.FailedDependency, "the root role was not found in upstream keyserver")
  val NotFound = RawError(ErrorCode("repo_resource_not_found"), StatusCodes.NotFound, "the requested repo resource was not found")
  val RepoConflict = RawError(ErrorCode("repo_conflict"), StatusCodes.Conflict, "repo already exists")
  val PrivateKeysNotInKeyserver = RawError(ErrorCode("private_keys_not_found"), StatusCodes.PreconditionFailed, "could not find required private keys. The repository might be using offline signing")
}


trait ReposerverClient {
  protected def ReposerverError(msg: String) = RawError(ErrorCode("reposerver_remote_error"), StatusCodes.BadGateway, msg)

  def createRoot(namespace: Namespace, keyType: KeyType): Future[RepoId]

  def fetchRoot(namespace: Namespace): Future[SignedPayload[RootRole]]

  def repoExists(namespace: Namespace)(implicit ec: ExecutionContext): Future[Boolean] =
    fetchRoot(namespace).transform {
      case Success(_) | Failure(KeysNotReady) => Success(true)
      case Failure(NotFound) | Failure(RootNotInKeyserver) => Success(false)
      case Failure(t) => Failure(t)
    }

  def addTarget(namespace: Namespace, fileName: String, uri: Uri, checksum: Checksum, length: Int,
                targetFormat: TargetFormat, name: Option[TargetName] = None, version: Option[TargetVersion] = None,
                hardwareIds: Seq[HardwareIdentifier] = Seq.empty): Future[Unit]

  def addTargetFromContent(namespace: Namespace, fileName: String, uri: Option[Uri], checksum: Checksum, length: Int,
                           targetFormat: TargetFormat,
                           content: Source[ByteString, Any],
                           name: TargetName, version: TargetVersion,
                           hardwareIds: Seq[HardwareIdentifier] = Seq.empty): Future[Unit]
}

object ReposerverHttpClient extends ServiceHttpClientSupport {
  def apply(reposerverUri: Uri, authHeaders: Option[HttpHeader] = None)
           (implicit ec: ExecutionContext, system: ActorSystem, mat: Materializer, tracing: RequestTracing): ReposerverHttpClient =
    new ReposerverHttpClient(reposerverUri, defaultHttpClient, authHeaders)
}


class ReposerverHttpClient(reposerverUri: Uri, httpClient: HttpRequest => Future[HttpResponse], authHeaders: Option[HttpHeader] = None)
                          (implicit ec: ExecutionContext, system: ActorSystem, mat: Materializer, tracing: RequestTracing)
  extends TracingHttpClient(httpClient) with ReposerverClient {

  import ReposerverClient._
  import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
  import io.circe.syntax._

  private def apiUri(path: Path) =
    reposerverUri.withPath(Path("/api") / "v1" ++ Slash(path))

  override def createRoot(namespace: Namespace, keyType: KeyType): Future[RepoId] =
    Marshal(CreateRepositoryRequest(keyType)).to[RequestEntity].flatMap { entity =>
      val req = HttpRequest(HttpMethods.POST, uri = apiUri(Path("user_repo")), entity = entity)
      execHttpWithNamespace[RepoId](namespace, req) {
        case error if error.status == StatusCodes.Conflict =>
          Future.failed(RepoConflict)
        case error if error.status == StatusCodes.Locked =>
          Future.failed(KeysNotReady)
      }
    }

  override def fetchRoot(namespace: Namespace): Future[SignedPayload[RootRole]] = {
    val req = HttpRequest(HttpMethods.GET, uri = apiUri(Path("user_repo/root.json")))
    execHttpWithNamespace[SignedPayload[RootRole]](namespace, req) {
      case error if error.status == StatusCodes.NotFound =>
        Future.failed(NotFound)
      case error if error.status == StatusCodes.Locked =>
        Future.failed(KeysNotReady)
      case error if error.status == StatusCodes.FailedDependency =>
        Future.failed(RootNotInKeyserver)
    }
  }

  private def addTargetErrorHandler[T]: PartialFunction[RemoteServiceError, Future[T]] = {
    case error if error.status == StatusCodes.PreconditionFailed =>
      Future.failed(PrivateKeysNotInKeyserver)
    case error if error.status == StatusCodes.NotFound =>
      Future.failed(NotFound)
    case error if error.status == StatusCodes.Locked =>
      Future.failed(KeysNotReady)
  }

  override def addTarget(namespace: Namespace, fileName: String,
                         uri: Uri, checksum: Checksum, length: Int,
                         targetFormat: TargetFormat,
                         name: Option[TargetName] = None, version: Option[TargetVersion] = None,
                         hardwareIds: Seq[HardwareIdentifier] = Seq.empty
                        ): Future[Unit] = {
    val payload = payloadFrom(uri, checksum, length, name, version, hardwareIds, targetFormat)

    val entity = HttpEntity(ContentTypes.`application/json`, payload.noSpaces)

    val req = HttpRequest(HttpMethods.POST,
      uri = apiUri(Path("user_repo") / "targets" / fileName),
      entity = entity)

    execHttpWithNamespace[Unit](namespace, req)(addTargetErrorHandler)
  }

  def execHttpWithNamespace[T: ClassTag : FromEntityUnmarshaller](namespace: Namespace, request: HttpRequest)
                                                                 (errorHandler: PartialFunction[RemoteServiceError, Future[T]] = PartialFunction.empty): Future[T] = {
    val req = request.addHeader(RawHeader("x-ats-namespace", namespace.get))

    val authReq = authHeaders match {
      case Some(a) => req.addHeader(a)
      case None => req
    }

    execHttp[T](authReq)(errorHandler)
  }

  private def payloadFrom(uri: Uri, checksum: Checksum, length: Int, name: Option[TargetName],
                          version: Option[TargetVersion], hardwareIds: Seq[HardwareIdentifier], targetFormat: TargetFormat): Json =
    RequestTargetItem(uri, checksum, Some(targetFormat), name, version, hardwareIds, length).asJson

  override def addTargetFromContent(namespace: Namespace, fileName: String, uri: Option[Uri],
                                    checksum: Checksum, length: Int, targetFormat: TargetFormat, content: Source[ByteString, Any],
                                    name: TargetName, version: TargetVersion,
                                    hardwareIds: Seq[HardwareIdentifier]): Future[Unit] = {
    val params =
      Map(
        "name" -> name.value, "version" -> version.value,
        "targetFormat" -> targetFormat.toString)

    val hwparams =
      if (hardwareIds.isEmpty)
        Map.empty
      else
        Map("hardwareIds" -> hardwareIds.map(_.value).mkString(","))

    val uri = apiUri(Path("user_repo") / "targets" / fileName).withQuery(Query(params ++ hwparams))

    val multipartForm =
      Multipart.FormData(Multipart.FormData.BodyPart("file",
        HttpEntity(ContentTypes.`application/octet-stream`, length, content), Map("filename" -> fileName)))

    Marshal(multipartForm).to[RequestEntity].flatMap { form =>
      val req = HttpRequest(HttpMethods.PUT, uri, entity = form)
      execHttpWithNamespace[Unit](namespace, req)(addTargetErrorHandler)
    }
  }
}

