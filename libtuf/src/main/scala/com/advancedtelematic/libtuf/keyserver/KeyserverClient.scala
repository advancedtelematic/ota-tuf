package com.advancedtelematic.libtuf.keyserver

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{StatusCodes, _}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import cats.syntax.show.toShowOps
import com.advancedtelematic.libats.http.ErrorCode
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, SignedPayload}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType._
import io.circe.{Encoder, Json}

import scala.concurrent.Future
import scala.reflect.ClassTag
import com.advancedtelematic.libtuf.data.TufCodecs.signedPayloadDecoder

trait KeyserverClient {
  val RootRoleNotFound = RawError(ErrorCode("root_role_not_found"), StatusCodes.FailedDependency, "root role was not found in upstream key store")
  val RootRoleConflict = RawError(ErrorCode("root_role_conflict"), StatusCodes.Conflict, "root role already exists")

  def createRoot(repoId: RepoId): Future[Json]

  def sign[T : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[Json]]

  def fetchRootRole(repoId: RepoId): Future[SignedPayload[Json]]
}

class KeyserverHttpClient(uri: Uri)(implicit system: ActorSystem, mat: ActorMaterializer) extends KeyserverClient {
  import io.circe.syntax._
  import system.dispatcher

  private val _http = Http()

  import de.heikoseeberger.akkahttpcirce.CirceSupport._

  private def KeyStoreError(msg: String) = RawError(ErrorCode("key_store_remote_error"), StatusCodes.BadGateway, msg)

  override def createRoot(repoId: RepoId): Future[Json] = {
    val entity = HttpEntity(ContentTypes.`application/json`, Json.obj("threshold" -> Json.fromInt(1)).noSpaces)
    val req = HttpRequest(HttpMethods.POST, uri = uri.withPath(uri.path / "root" / repoId.show), entity = entity)

    execHttp[Json](req){
      case response if response.status == StatusCodes.Conflict =>
        Future.failed(RootRoleConflict)
    }
  }

  override def sign[T : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[Json]] = {
    val entity = HttpEntity(ContentTypes.`application/json`, payload.asJson.noSpaces)
    val req = HttpRequest(HttpMethods.POST, uri = uri.withPath(uri.path / "root" / repoId.show / roleType.show), entity = entity)

    execHttp[SignedPayload[Json]](req)()
  }

  override def fetchRootRole(repoId: RepoId): Future[SignedPayload[Json]] = {
    val req = HttpRequest(HttpMethods.GET, uri = uri.withPath(uri.path / "root" / repoId.show))

    execHttp[SignedPayload[Json]](req) {
      case response if response.status == StatusCodes.NotFound =>
        Future.failed(RootRoleNotFound)
    }
  }

  private def defaultErrorHandler[T](): PartialFunction[HttpResponse, Future[T]] = PartialFunction.empty

  private def execHttp[T : ClassTag](request: HttpRequest)
                                    (errorHandler: PartialFunction[HttpResponse, Future[T]] = defaultErrorHandler())
                                    (implicit um: FromEntityUnmarshaller[T]): Future[T] = {
    _http.singleRequest(request).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case r =>
        if(errorHandler.isDefinedAt(r))
          errorHandler(r)
        else
          FastFuture.failed(KeyStoreError(s"Unexpected response from RoleKeyStore: $r"))
    }
  }
}
