package com.advancedtelematic.tuf.reposerver.http

import java.io.{ByteArrayOutputStream, OutputStream}
import java.net.{HttpURLConnection, URL}

import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.HttpHeader.ParsingResult.{Error, Ok}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import com.advancedtelematic.tuf.reposerver.util.ResourceSpec

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}

private class FakeHttpConnection extends HttpURLConnection(new URL("http://fakeHttpConnection/test")) {
  val requestBodyBuffer = new ByteArrayOutputStream()

  override def disconnect(): Unit = ()

  override def usingProxy(): Boolean = false

  override def connect(): Unit = ()

  override def getOutputStream: OutputStream = requestBodyBuffer
}

trait FakeScalajHttpClient {
  self: ResourceSpec ⇒

  private def toAkkaHttpMethod(method: String): HttpMethod = method.toUpperCase match {
    case "GET" ⇒ HttpMethods.GET
    case "PUT" ⇒ HttpMethods.PUT
    case "POST" ⇒ HttpMethods.POST
    case "DELETE" ⇒ HttpMethods.DELETE
    case "CONNECT" ⇒ HttpMethods.CONNECT
    case "HEAD" ⇒ HttpMethods.HEAD
    case "OPTIONS" ⇒ HttpMethods.OPTIONS
    case "PATCH" ⇒ HttpMethods.PATCH
    case "TRACE" ⇒ HttpMethods.TRACE
    case _ ⇒ throw new IllegalArgumentException(s"unknown http method: $method")
  }

  private def toAkkaRequest(req: scalaj.http.HttpRequest): HttpRequest = {
    val akkaHeaders = req.headers.flatMap { case (n, v) ⇒
      HttpHeader.parse(n, v) match {
        case Ok(header, _) ⇒ Seq(header)
        case Error(_) ⇒ Seq.empty
      }
    }

    val fakeHttpConnection = new FakeHttpConnection()

    req.connectFunc(req, fakeHttpConnection)

    val akkaRequest = HttpRequest(toAkkaHttpMethod(req.method), uri = Uri(req.url), collection.immutable.Seq(akkaHeaders: _*))

    val contentType = akkaRequest.header[`Content-Type`].map(_.contentType).getOrElse(ContentTypes.NoContentType)

    val entity = HttpEntity(contentType, fakeHttpConnection.requestBodyBuffer.toByteArray)

    akkaRequest.withEntity(entity)
  }

  private def toShttpjResponse(resp: HttpResponse): Future[scalaj.http.HttpResponse[Array[Byte]]] = {
    val responseHeaders = resp.headers.map { h ⇒ h.name → IndexedSeq(h.value) }.toMap

    val reponseEntityFut = resp.entity.toStrict(FiniteDuration(10, scala.concurrent.duration.SECONDS))

    reponseEntityFut.map(_.data.toArray).map { bytes ⇒
      scalaj.http.HttpResponse(bytes, resp.status.intValue(), responseHeaders)
    }
  }

  def testClient(req: scalaj.http.HttpRequest): Future[scalaj.http.HttpResponse[Array[Byte]]] = {
    val promise = Promise[scalaj.http.HttpResponse[Array[Byte]]]()

     toAkkaRequest(req) ~> Route.seal(routes) ~> check {
       promise.completeWith(toShttpjResponse(response))
    }

    promise.future
  }
}
