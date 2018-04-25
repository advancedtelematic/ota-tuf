package com.advancedtelematic.libtuf_server.http

import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import com.advancedtelematic.libats.data.ErrorRepresentation
import com.advancedtelematic.libats.http.Errors
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.{Encoder, Json}
import org.slf4j.LoggerFactory
import cats.syntax.option._

import scala.concurrent.Future
import scala.reflect.ClassTag


trait ServiceHttpClientSupport {
  def defaultHttpClient(implicit system: ActorSystem, mat: Materializer): (HttpRequest => Future[HttpResponse]) = {
    val _http = Http()
    req => _http.singleRequest(req)
  }
}

// TODO: Move to libats?
abstract class ServiceHttpClient(httpClient: HttpRequest => Future[HttpResponse])
                                (implicit system: ActorSystem, mat: Materializer) {
  import io.circe.syntax._
  import system.dispatcher
  import Errors.RemoteServiceError

  private val log = LoggerFactory.getLogger(this.getClass)

  protected implicit val unitFromEntityUnmarshaller: FromEntityUnmarshaller[Unit] = Unmarshaller.strict(_.discardBytes())

  private def defaultErrorHandler[T](): PartialFunction[RemoteServiceError, Future[T]] = PartialFunction.empty

  protected def execJsonHttp[Res : ClassTag : FromEntityUnmarshaller, Req : Encoder]
  (request: HttpRequest, entity: Req)
  (errorHandler: PartialFunction[RemoteServiceError, Future[Res]] = defaultErrorHandler()): Future[Res] = {
    val httpEntity = HttpEntity(ContentTypes.`application/json`, entity.asJson.noSpaces)
    val req = request.withEntity(httpEntity)
    execHttp(req)(errorHandler)
  }

  private def tryErrorParsing(response: HttpResponse)(implicit um: FromEntityUnmarshaller[ErrorRepresentation]): Future[RemoteServiceError] = {
    um(response.entity).map { rawError =>
      RemoteServiceError(s"${rawError.description}", response.status, rawError.cause.getOrElse(Json.Null),
        rawError.code, rawError.some, rawError.errorId.getOrElse(UUID.randomUUID()))
    }.recoverWith { case _ =>
      Unmarshaller.stringUnmarshaller(response.entity).map(msg => RemoteServiceError(msg, response.status))
    }.recover { case _ =>
      RemoteServiceError(s"Unknown error: $response", response.status)
    }
  }

  protected def execHttp[T : ClassTag](request: HttpRequest)
                                      (errorHandler: PartialFunction[RemoteServiceError, Future[T]] = defaultErrorHandler())
                                      (implicit um: FromEntityUnmarshaller[T]): Future[T] =
    httpClient(request).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case r =>
        tryErrorParsing(r).flatMap { error =>
          if (errorHandler.isDefinedAt(error))
            errorHandler(error).recoverWith { case ex => r.discardEntityBytes(); Future.failed(ex) }
          else {
            log.debug(s"request failed: $request")
            val e = error.copy(msg = s"${this.getClass.getSimpleName}|Unexpected response from remote server at ${request.uri}|${request.method.value}|${r.status.intValue()}|${error.msg}")
            FastFuture.failed(e)
          }
        }
    }
}
