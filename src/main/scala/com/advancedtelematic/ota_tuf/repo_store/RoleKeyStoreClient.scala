package com.advancedtelematic.ota_tuf.repo_store

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{StatusCodes, _}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import cats.syntax.show.toShowOps
import com.advancedtelematic.ota_tuf.data.ClientDataType.SignedPayload
import com.advancedtelematic.ota_tuf.data.Codecs._
import com.advancedtelematic.ota_tuf.data.DataType.RepoId
import com.advancedtelematic.ota_tuf.data.RoleType.RoleType
import io.circe.{Encoder, Json}
import org.genivi.sota.http.Errors.RawError
import org.genivi.sota.rest.ErrorCode
import scala.concurrent.Future
import scala.reflect.ClassTag

trait RoleKeyStoreClient {
  val RootRoleNotFound = RawError(ErrorCode("root_role_not_found"), StatusCodes.FailedDependency, "root role was not found in upstream key store")

  def sign[T : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[Json]]

  def fetchRootRole(repoId: RepoId): Future[SignedPayload[Json]]
}

class RoleKeyStoreHttpClient(uri: Uri)(implicit system: ActorSystem, mat: ActorMaterializer) extends RoleKeyStoreClient {
  import io.circe.syntax._
  import system.dispatcher

  private val _http = Http()

  import de.heikoseeberger.akkahttpcirce.CirceSupport._

  private def KeyStoreError(msg: String) = RawError(ErrorCode("key_store_remote_error"), StatusCodes.BadGateway, msg)

  override def sign[T : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[Json]] = {
    val entity = HttpEntity(ContentTypes.`application/json`, payload.asJson.noSpaces)
    val req = HttpRequest(HttpMethods.POST, uri = uri.withPath(uri.path / repoId.show / roleType.toString), entity = entity)

    execHttp[SignedPayload[Json]](req)()
  }

  override def fetchRootRole(repoId: RepoId): Future[SignedPayload[Json]] = {
    val req = HttpRequest(HttpMethods.GET, uri = uri.withPath(uri.path / repoId.show))

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
