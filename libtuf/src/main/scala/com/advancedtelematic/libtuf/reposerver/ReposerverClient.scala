package com.advancedtelematic.libtuf.reposerver

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, HardwareIdentifier, RepoId, TargetName, TargetVersion}
import io.circe.{Decoder, Encoder, Json}
import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.Uri.Path.Slash
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import com.advancedtelematic.libats.data.Namespace
import com.advancedtelematic.libats.http.ErrorCode
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.codecs.CirceRefined._
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat._

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import io.circe.generic.semiauto._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.reposerver.ReposerverClient.RequestTargetItem

object ReposerverClient {
  import com.advancedtelematic.libats.codecs.CirceAnyVal._

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
}

trait ReposerverClient {
  protected def ReposerverError(msg: String) = RawError(ErrorCode("reposerver_remote_error"), StatusCodes.BadGateway, msg)
  val RepoConflict   = RawError(ErrorCode("repo_conflict"), StatusCodes.Conflict, "repo already exists")

  def createRoot(namespace: Namespace): Future[RepoId]

  def addTarget(namespace: Namespace, fileName: String, uri: Uri, checksum: Checksum, length: Int,
                targetFormat: TargetFormat, name: Option[TargetName] = None, version: Option[TargetVersion] = None,
                hardwareIds: Seq[HardwareIdentifier] = Seq.empty): Future[Unit]
}

final case class NoContent()

object NoContent {
  import akka.http.scaladsl.unmarshalling.Unmarshaller
  implicit def noContentUnmarshaller: FromEntityUnmarshaller[NoContent] = Unmarshaller {
    implicit ec => response => FastFuture.successful(NoContent())
  }
}

class ReposerverHttpClient(reposerverUri: Uri)
                          (implicit ec: ExecutionContext, system: ActorSystem, mat: Materializer)
  extends ReposerverClient {

  import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
  import io.circe.syntax._

  private def apiUri(path: Path) =
    reposerverUri.withPath(Path("/api") / "v1" ++ Slash(path))

  private val _http = Http()

  override def createRoot(namespace: Namespace): Future[RepoId] = {
    val req = HttpRequest(HttpMethods.POST, uri = apiUri(Path("user_repo")))
    execHttp[RepoId](namespace, req) {
      case response if response.status == StatusCodes.Conflict =>
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

    execHttp[NoContent](namespace, req)().map(_ => ())
  }

  private def payloadFrom(uri: Uri, checksum: Checksum, length: Int, name: Option[TargetName],
                          version: Option[TargetVersion], hardwareIds: Seq[HardwareIdentifier], targetFormat: TargetFormat): Json =
    RequestTargetItem(uri, checksum, Some(targetFormat), name, version, hardwareIds, length).asJson

  private def defaultErrorHandler[T](): PartialFunction[HttpResponse, Future[T]] = PartialFunction.empty

  private def execHttp[T : ClassTag](namespace: Namespace, request: HttpRequest)
                                    (errorHandler: PartialFunction[HttpResponse, Future[T]] = defaultErrorHandler())
                                    (implicit um: FromEntityUnmarshaller[T]): Future[T] = {
    _http.singleRequest(request.withHeaders(RawHeader("x-ats-namespace", namespace.get))).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case r =>
        if(errorHandler.isDefinedAt(r))
          errorHandler(r)
        else
          FastFuture.failed(ReposerverError(s"Unexpected response from Reposerver: $r"))
    }
 }
}
