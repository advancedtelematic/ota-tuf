package com.advancedtelematic.libtuf_server.reposerver

import akka.http.scaladsl.model._
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, RepoId, SignedPayload, TargetName, TargetVersion}
import io.circe.{Decoder, Encoder, Json}
import akka.actor.ActorSystem
import RepoId._
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.Uri.{Path, Query}
import akka.http.scaladsl.model.Uri.Path.Slash
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.advancedtelematic.libats.data.DataType.{Checksum, Namespace}
import com.advancedtelematic.libats.data.ErrorCode
import com.advancedtelematic.libats.http.Errors.{JsonError, RawError, RemoteServiceError}
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat._
import com.advancedtelematic.libtuf.data.TufCodecs._

import scala.concurrent.{ExecutionContext, Future}
import io.circe.generic.semiauto._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.http.HttpCodecs._
import cats.syntax.show._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf_server.http.{ServiceHttpClient, ServiceHttpClientSupport}

import scala.reflect.ClassTag

object ReposerverClient {
  import com.advancedtelematic.libats.codecs.CirceCodecs._

  object RequestTargetItem {
    implicit val encoder: Encoder[RequestTargetItem] = deriveEncoder
    implicit val decoder: Decoder[RequestTargetItem] = deriveDecoder
  }

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

  def createRoot(namespace: Namespace): Future[RepoId]

  def fetchRoot(namespace: Namespace): Future[SignedPayload[RootRole]]

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
           (implicit ec: ExecutionContext, system: ActorSystem, mat: Materializer): ReposerverHttpClient =
    new ReposerverHttpClient(reposerverUri, defaultHttpClient, authHeaders)
}


class ReposerverHttpClient(reposerverUri: Uri, httpClient: HttpRequest => Future[HttpResponse], authHeaders: Option[HttpHeader] = None)
                          (implicit ec: ExecutionContext, system: ActorSystem, mat: Materializer)
  extends ServiceHttpClient(httpClient) with ReposerverClient {

  import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
  import io.circe.syntax._
  import ReposerverClient._

  private def apiUri(path: Path) =
    reposerverUri.withPath(Path("/api") / "v1" ++ Slash(path))

  override def createRoot(namespace: Namespace): Future[RepoId] = {
    val req = HttpRequest(HttpMethods.POST, uri = apiUri(Path("user_repo")))
    execHttpWithNamespace[RepoId](namespace, req) {
      case error if error.status == StatusCodes.Conflict =>
        Future.failed(RepoConflict)
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

  def execHttpWithNamespace[T : ClassTag : FromEntityUnmarshaller](namespace: Namespace, request: HttpRequest)
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
      if(hardwareIds.isEmpty)
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

