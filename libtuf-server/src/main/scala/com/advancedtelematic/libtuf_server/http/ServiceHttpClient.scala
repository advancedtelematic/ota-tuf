package com.advancedtelematic.libtuf_server.http

import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import cats.syntax.either._
import com.advancedtelematic.libats.data.ErrorRepresentation
import com.advancedtelematic.libats.http.Errors
import com.advancedtelematic.libats.http.Errors.JsonError
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
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

  private def defaultErrorHandler[T](): PartialFunction[StatusCode, Future[T]] = PartialFunction.empty

  protected def execJsonHttp[Res : ClassTag : FromEntityUnmarshaller, Req : Encoder]
  (request: HttpRequest, entity: Req)
  (errorHandler: PartialFunction[StatusCode, Future[Res]] = defaultErrorHandler()): Future[Res] = {
    val httpEntity = HttpEntity(ContentTypes.`application/json`, entity.asJson.noSpaces)
    val req = request.withEntity(httpEntity)
    execHttp(req)(errorHandler)
  }

  private def tryErrorParsing(response: HttpResponse)(implicit um: FromEntityUnmarshaller[ErrorRepresentation]): Future[JsonError] = {
    um(response.entity).map { rawError =>
      RemoteServiceError(s"${rawError.description}", rawError.asJson.some, rawError.errorId.getOrElse(UUID.randomUUID()))
    }.recoverWith { case _ =>
      FailFastCirceSupport.jsonUnmarshaller(response.entity).flatMap { json =>
        Future.fromTry {
          json.hcursor.downField("errors").as[List[String]].toTry.map { errors =>
            RemoteServiceError(s"http/${response.status}", errors.asJson.some)
          }
        }
      }
    }.recoverWith { case _ =>
      Unmarshaller.stringUnmarshaller(response.entity).map(msg => Errors.RemoteServiceError(msg))
    }.recover { case _ =>
      RemoteServiceError(s"Unknown error: $response")
    }
  }

  protected def execHttp[T : ClassTag](request: HttpRequest)
                                      (errorHandler: PartialFunction[StatusCode, Future[T]] = defaultErrorHandler())
                                      (implicit um: FromEntityUnmarshaller[T]): Future[T] =
    httpClient(request).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case r =>
        if (errorHandler.isDefinedAt(r.status))
          errorHandler(r.status).map { t => r.discardEntityBytes() ; t }.recoverWith { case ex => r.discardEntityBytes(); Future.failed(ex) }
        else
          tryErrorParsing(r).flatMap { error =>
            log.debug(s"request failed: $request")
            val e = error.copy(msg = s"${this.getClass.getSimpleName}|Unexpected response from remote server at ${request.uri}|${r.status.intValue()}|${error.msg}")
            FastFuture.failed(e)
          }
    }
}
