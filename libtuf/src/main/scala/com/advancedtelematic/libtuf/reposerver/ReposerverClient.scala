package com.advancedtelematic.libtuf.reposerver

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, RepoId}
import io.circe.{Json, JsonObject}
import RepoId._
import akka.actor.ActorSystem
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import cats.syntax.show._
import com.advancedtelematic.libats.http.ErrorCode
import com.advancedtelematic.libats.http.Errors.RawError

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

trait ReposerverClient {
  protected def KeyStoreError(msg: String) = RawError(ErrorCode("reposerver_remote_error"), StatusCodes.BadGateway, msg)

  def createRoot(repoId: RepoId): Future[Unit]

  def addTarget(repoId: RepoId, fileName: String, uri: Uri, checksum: Checksum, length: Int): Future[Unit]
}

class ReposerverHttpClient(reposerverUri: Uri)
                          (implicit ec: ExecutionContext, system: ActorSystem, mat: Materializer)
  extends ReposerverClient {

  import de.heikoseeberger.akkahttpcirce.CirceSupport._
  import com.advancedtelematic.libats.codecs.AkkaCirce.uriEncoder
  import com.advancedtelematic.libtuf.data.TufCodecs.checkSumEncoder
  import io.circe.syntax._

  private val _uri = reposerverUri

  private val _http = Http()

  override def createRoot(repoId: RepoId): Future[Unit] = {
    val req = HttpRequest(HttpMethods.POST, uri = _uri.withPath(_uri.path / "repo" / repoId.show))
    execHttp[Unit](req)
  }

  override def addTarget(repoId: RepoId, fileName: String, uri: Uri, checksum: Checksum, length: Int): Future[Unit] = {
    val payload = JsonObject.fromIterable(List(
      "uri" -> uri.asJson,
      "checksum" -> checksum.asJson,
      "length" -> length.asJson)
    )

    val entity = HttpEntity(ContentTypes.`application/json`, payload.asJson.noSpaces)

    val req = HttpRequest(HttpMethods.POST,
      uri = _uri.withPath(_uri.path / "repo" / repoId.show / "targets" / fileName),
      entity = entity)

    execHttp[Unit](req)
  }

  private def execHttp[T : ClassTag](request: HttpRequest)
                                    (implicit um: FromEntityUnmarshaller[T]): Future[T] = {
    _http.singleRequest(request).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case r =>
          FastFuture.failed(KeyStoreError(s"Unexpected response from RoleKeyStore: $r"))
    }
  }
}
