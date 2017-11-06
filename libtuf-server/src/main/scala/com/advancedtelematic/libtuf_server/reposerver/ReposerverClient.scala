package com.advancedtelematic.libtuf_server.reposerver

import akka.http.scaladsl.model._
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, RepoId, TargetName, TargetVersion}
import io.circe.{Decoder, Encoder, Json}
import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.Uri.Path.Slash
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.Materializer
import com.advancedtelematic.libats.data.DataType.{Checksum, Namespace}
import com.advancedtelematic.libats.data.ErrorCode
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat._

import scala.concurrent.{ExecutionContext, Future}
import io.circe.generic.semiauto._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.http.HttpCodecs._
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

  val RepoConflict   = RawError(ErrorCode("repo_conflict"), StatusCodes.Conflict, "repo already exists")
  val OfflineKey = RawError(ErrorCode("offline_key"), StatusCodes.PreconditionFailed, "repo is using offline signing, can't add targets online")
  val UserRepoNotFound = RawError(ErrorCode("user_repo_not_found"), StatusCodes.NotFound, "the repo does not exist yet")
}


trait ReposerverClient {
  protected def ReposerverError(msg: String) = RawError(ErrorCode("reposerver_remote_error"), StatusCodes.BadGateway, msg)

  def createRoot(namespace: Namespace): Future[RepoId]

  def addTarget(namespace: Namespace, fileName: String, uri: Uri, checksum: Checksum, length: Int,
                targetFormat: TargetFormat, name: Option[TargetName] = None, version: Option[TargetVersion] = None,
                hardwareIds: Seq[HardwareIdentifier] = Seq.empty): Future[Unit]

  def targets(namespace: Namespace, fileName: String, uri: Uri, checksum: Checksum, length: Int): Unit = {

  }
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
      case status if status == StatusCodes.Conflict =>
        Future.failed(RepoConflict)
    }
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

    execHttpWithNamespace[Unit](namespace, req) {
      case status if status == StatusCodes.PreconditionFailed =>
        Future.failed(OfflineKey)
      case status if status == StatusCodes.NotFound =>
        Future.failed(UserRepoNotFound)
    }
  }

  def execHttpWithNamespace[T : ClassTag : FromEntityUnmarshaller](namespace: Namespace, request: HttpRequest)
                                                                  (errorHandler: PartialFunction[StatusCode, Future[T]]): Future[T] = {
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
}

