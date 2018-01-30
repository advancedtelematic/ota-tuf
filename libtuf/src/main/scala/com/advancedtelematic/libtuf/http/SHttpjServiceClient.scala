package com.advancedtelematic.libtuf.http

import io.circe.{Decoder, Encoder, Json}
import cats.syntax.either._
import com.advancedtelematic.libats.data
import com.advancedtelematic.libats.data.{ErrorCode, ErrorCodes, ErrorRepresentation}
import com.advancedtelematic.libtuf.http.SHttpjServiceClient.{HttpResponse, HttpjClientError}

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scalaj.http.{HttpRequest, HttpResponse => ScalaJHttpResponse}
import io.circe.syntax._
import org.slf4j.LoggerFactory

import scala.util.control.NoStackTrace

object SHttpjServiceClient {

  case class HttpjClientError(msg: String) extends Exception(s"remote_service_error: $msg") with NoStackTrace

  case class HttpResponse[T](body: T, response: ScalaJHttpResponse[Array[Byte]])

  def UnknownErrorRepr(msg: String) = ErrorRepresentation(data.ErrorCode("unknown_error_repr"), msg)
}

abstract class SHttpjServiceClient(client: scalaj.http.HttpRequest ⇒ Future[ScalaJHttpResponse[Array[Byte]]])
                                  (implicit ec: ExecutionContext) {

  import SHttpjServiceClient.UnknownErrorRepr

  private val log = LoggerFactory.getLogger(this.getClass)

  private def defaultErrorHandler[T](): PartialFunction[(Int, ErrorRepresentation), Future[T]] = PartialFunction.empty

  protected def execJsonHttp[Res: ClassTag : Decoder, Req: Encoder]
  (request: HttpRequest, entity: Req)
  (errorHandler: PartialFunction[(Int, ErrorRepresentation), Future[Res]] = defaultErrorHandler()): Future[Res] = {

    val req = request
      .postData(entity.asJson.noSpaces.getBytes)
      .header("Content-Type", "application/json")
      .method(request.method)

    execHttp(req)(errorHandler).map(_.body)
  }

  private def tryErrorParsing(response: ScalaJHttpResponse[Array[Byte]]): ErrorRepresentation = {
    def fallbackToResponseParse: String = {
      tryParseResponse[Json](response).flatMap { json ⇒
        json.hcursor.downField("errors").as[List[String]].map(_.mkString).toTry
      }.recover {
        case _ => new String(response.body)
      }.map { errorRepr =>
        s"http/${response.code}: $errorRepr"
      }.getOrElse {
        s"Unknown error: $response"
      }
    }

    tryParseResponse[ErrorRepresentation](response).getOrElse(UnknownErrorRepr(fallbackToResponseParse))
  }

  def tryParseResponse[T: ClassTag : Decoder](response: ScalaJHttpResponse[Array[Byte]]): Try[T] =
    if (implicitly[ClassTag[T]].runtimeClass.equals(classOf[Unit]))
      Success(()).asInstanceOf[Try[T]]
    else
      io.circe.parser.parse(new String(response.body)).flatMap(_.as[T]).toTry

  protected def execHttp[T: ClassTag : Decoder](request: HttpRequest)
                                               (errorHandler: PartialFunction[(Int, ErrorRepresentation), Future[T]] = defaultErrorHandler()): Future[HttpResponse[T]] =
    client(request).flatMap { resp ⇒
      if (resp.isSuccess)
        Future.fromTry(tryParseResponse[T](resp).map(parsed => HttpResponse(parsed, resp)))
      else {
        val parsedErr = tryErrorParsing(resp)

        if (errorHandler.isDefinedAt(resp.code, parsedErr)) {
          errorHandler(resp.code, parsedErr).map(HttpResponse(_, resp))
        } else {
          log.debug(s"request failed: $request")
          Future.failed {
            HttpjClientError(s"${this.getClass.getSimpleName}|Unexpected response from remote server at ${request.method}|${request.url}|$parsedErr")
          }
        }
      }
    }
}
