package com.advancedtelematic.libtuf.reposerver

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, RepoId}
import io.circe.JsonObject
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

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

trait ReposerverClient {
  protected def KeyStoreError(msg: String) = RawError(ErrorCode("reposerver_remote_error"), StatusCodes.BadGateway, msg)

  def createRoot(namespace: Namespace): Future[RepoId]

  def addTarget(namespace: Namespace, fileName: String, uri: Uri, checksum: Checksum, length: Int): Future[Unit]
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
  import com.advancedtelematic.libtuf.data.TufCodecs.uriEncoder
  import com.advancedtelematic.libtuf.data.TufCodecs.checkSumEncoder
  import io.circe.syntax._

  private def apiUri(path: Path) =
    reposerverUri.withPath(Path("/api") / "v1" ++ Slash(path))

  private val _http = Http()

  override def createRoot(namespace: Namespace): Future[RepoId] = {
    val req = HttpRequest(HttpMethods.POST, uri = apiUri(Path("user_repo")))
    execHttp[RepoId](namespace, req)
  }

  override def addTarget(namespace: Namespace, fileName: String, uri: Uri, checksum: Checksum, length: Int): Future[Unit] = {
    val payload = JsonObject.fromIterable(List(
      "uri" -> uri.asJson,
      "checksum" -> checksum.asJson,
      "length" -> length.asJson))

    val entity = HttpEntity(ContentTypes.`application/json`, payload.asJson.noSpaces)

    val req = HttpRequest(HttpMethods.POST,
      uri = apiUri(Path("user_repo") / "targets" / fileName),
      entity = entity)

    execHttp[NoContent](namespace, req).map(_ => ())
  }

  private def execHttp[T : ClassTag](namespace: Namespace, request: HttpRequest)
                                    (implicit um: FromEntityUnmarshaller[T]): Future[T] = {
    _http.singleRequest(request.withHeaders(RawHeader("x-ats-namespace", namespace.get))).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case r =>
          FastFuture.failed(KeyStoreError(s"Unexpected response from RoleKeyStore: $r"))
    }
  }
}
