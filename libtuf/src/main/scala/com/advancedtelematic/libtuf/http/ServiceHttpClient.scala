package com.advancedtelematic.libtuf.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import com.advancedtelematic.libats.http.ErrorCode
import com.advancedtelematic.libats.http.Errors.RawError
import io.circe.Encoder

import scala.concurrent.Future
import scala.reflect.ClassTag


trait ServiceHttpClientSupport {
  def defaultHttpClient(implicit system: ActorSystem, mat: ActorMaterializer): (HttpRequest => Future[HttpResponse]) = {
    val _http = Http()
    req => _http.singleRequest(req)
  }
}

// TODO: Move to libats?
abstract class ServiceHttpClient(baseUri: Uri, httpClient: HttpRequest => Future[HttpResponse])
                                (implicit system: ActorSystem, mat: ActorMaterializer) {
  import io.circe.syntax._
  import system.dispatcher

  private def RemoteServiceError(msg: String) = RawError(ErrorCode("remote_service_error"), StatusCodes.BadGateway, msg)

  protected implicit val unitFromEntityUnmarshaller: FromEntityUnmarshaller[Unit] = Unmarshaller.strict(_.discardBytes())

  private def defaultErrorHandler[T](): PartialFunction[StatusCode, Future[T]] = PartialFunction.empty

  protected def execJsonHttp[Res : ClassTag : FromEntityUnmarshaller, Req : Encoder]
  (request: HttpRequest, entity: Req)
  (errorHandler: PartialFunction[StatusCode, Future[Res]] = defaultErrorHandler()): Future[Res] = {
    val httpEntity = HttpEntity(ContentTypes.`application/json`, entity.asJson.noSpaces)
    val req = request.withEntity(httpEntity)
    execHttp(req)(errorHandler)
  }

  protected def execHttp[T : ClassTag](request: HttpRequest)
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
            FastFuture.failed(RemoteServiceError(s"Unexpected response from remote server: $r"))
        }

        failureF.map { t => r.discardEntityBytes() ; t }.recoverWith { case ex => r.discardEntityBytes(); Future.failed(ex) }
    }
  }
}
