package com.advancedtelematic.libtuf.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import com.advancedtelematic.libats.http.{ErrorCode, ErrorRepresentation}
import com.advancedtelematic.libats.http.Errors.RawError
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.{Decoder, Encoder}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import cats.syntax.either._
import ErrorRepresentation._
import akka.event.slf4j.Logger
import org.slf4j.{ILoggerFactory, LoggerFactory}

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

  private val log = LoggerFactory.getLogger(this.getClass)

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

  private def tryErrorParsing(response: HttpResponse)(implicit um: FromEntityUnmarshaller[ErrorRepresentation]): Future[String] = {
    um(response.entity).map { rawError =>
      s"${rawError.code.code}: ${rawError.description}"
    }.recoverWith { case _ =>
      FailFastCirceSupport.jsonUnmarshaller(response.entity).flatMap { json =>
        Future.fromTry(json.hcursor.downField("errors").as[List[String]].toTry.map(_.mkString))
      }
    }.recoverWith { case _ =>
      Unmarshaller.stringUnmarshaller(response.entity)
    }.map { errorRepr =>
      s"http/${response.status}: $errorRepr"
    }.recover { case _ =>
      s"Unknown error: $response"
    }
  }

  protected def execHttp[T : ClassTag](request: HttpRequest)
                                      (errorHandler: PartialFunction[StatusCode, Future[T]] = defaultErrorHandler())
                                      (implicit um: FromEntityUnmarshaller[T]): Future[T] = {
    httpClient(request).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case r =>
        if (errorHandler.isDefinedAt(r.status))
          errorHandler(r.status).map { t => r.discardEntityBytes() ; t }.recoverWith { case ex => r.discardEntityBytes(); Future.failed(ex) }
        else
          tryErrorParsing(r).flatMap { errorRepr =>
            log.debug(s"request failed: $request")
            FastFuture.failed(RemoteServiceError(s"${this.getClass.getSimpleName}|Unexpected response from remote server at ${request.uri}|$errorRepr"))
          }
    }
  }
}
