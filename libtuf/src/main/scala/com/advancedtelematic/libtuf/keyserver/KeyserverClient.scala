package com.advancedtelematic.libtuf.keyserver

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.Uri.Path.{Empty, Slash}
import akka.http.scaladsl.model.{StatusCodes, _}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import cats.syntax.show.toShowOps
import com.advancedtelematic.libats.http.ErrorCode
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{KeyType, RepoId, RsaKeyType, SignedPayload, TufKey}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType._
import io.circe.{Decoder, Encoder, Json}

import scala.concurrent.Future
import scala.reflect.ClassTag
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._

trait KeyserverClient {
  val RootRoleNotFound = RawError(ErrorCode("root_role_not_found"), StatusCodes.FailedDependency, "root role was not found in upstream key store")
  val RootRoleConflict = RawError(ErrorCode("root_role_conflict"), StatusCodes.Conflict, "root role already exists")

  def createRoot(repoId: RepoId, keyType: KeyType = RsaKeyType): Future[Json]

  def sign[T : Decoder : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]]

  def fetchRootRole(repoId: RepoId): Future[SignedPayload[RootRole]]

  def addTargetKey(repoId: RepoId, key: TufKey): Future[Unit]
}

object KeyserverHttpClient {

  def apply(uri: Uri)(implicit system: ActorSystem, mat: ActorMaterializer): KeyserverHttpClient = {
    val _http = Http()
    new KeyserverHttpClient(uri, req => _http.singleRequest(req))
  }
}

class KeyserverHttpClient(uri: Uri, httpClient: HttpRequest => Future[HttpResponse])
                         (implicit system: ActorSystem, mat: ActorMaterializer) extends KeyserverClient {
  import io.circe.syntax._
  import system.dispatcher

  import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._

  private def apiUri(path: Path) =
    uri.withPath(Empty / "api" / "v1" ++ Slash(path))

  private def KeyserverError(msg: String) = RawError(ErrorCode("keyserver_remote_error"), StatusCodes.BadGateway, msg)

  override def createRoot(repoId: RepoId, keyType: KeyType): Future[Json] = {
    val entity = HttpEntity(ContentTypes.`application/json`, Json.obj("threshold" -> 1.asJson, "keyType" -> keyType.asJson).noSpaces)
    val req = HttpRequest(HttpMethods.POST, uri = apiUri(Path("root") / repoId.show), entity = entity)

    execHttp[Json](req) {
      case StatusCodes.Conflict =>
        Future.failed(RootRoleConflict)
    }
  }

  override def sign[T : Decoder : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]] = {
    val entity = HttpEntity(ContentTypes.`application/json`, payload.asJson.noSpaces)
    val req = HttpRequest(HttpMethods.POST, uri = apiUri(Path("root") / repoId.show / roleType.show), entity = entity)

    execHttp[SignedPayload[T]](req)()
  }

  override def fetchRootRole(repoId: RepoId): Future[SignedPayload[RootRole]] = {
    val req = HttpRequest(HttpMethods.GET, uri = apiUri(Path("root") / repoId.show))

    execHttp[SignedPayload[RootRole]](req) {
      case StatusCodes.NotFound =>
        Future.failed(RootRoleNotFound)
    }
  }

  override def addTargetKey(repoId: RepoId, key: TufKey): Future[Unit] = {
    val entity = HttpEntity(ContentTypes.`application/json`, key.asJson.noSpaces)
    val req = HttpRequest(HttpMethods.PUT, uri = apiUri(Path("root") / repoId.show / "keys" / "targets")).withEntity(entity)
    execHttp[Unit](req)()
  }

  private implicit val unitFromEntityUnmarshaller: FromEntityUnmarshaller[Unit] = Unmarshaller.strict(_.discardBytes())

  private def defaultErrorHandler[T](): PartialFunction[StatusCode, Future[T]] = PartialFunction.empty

  private def execHttp[T : ClassTag](request: HttpRequest)
                                    (errorHandler: PartialFunction[StatusCode, Future[T]] = defaultErrorHandler())
                                    (implicit um: FromEntityUnmarshaller[T]): Future[T] = {
    httpClient(request).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case r =>
        val failureF = {
          if (errorHandler.isDefinedAt(r.status))
            errorHandler(r.status)
          else
            FastFuture.failed(KeyserverError(s"Unexpected response from Keyserver: $r"))
        }

        failureF.transform { t => r.discardEntityBytes() ; t }
    }
  }
}
