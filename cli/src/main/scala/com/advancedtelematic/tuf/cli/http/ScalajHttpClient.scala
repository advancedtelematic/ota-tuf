package com.advancedtelematic.tuf.cli.http

import scalaj.http.HttpRequest

import scala.concurrent.{ExecutionContext, Future}

class ScalajHttpClient(implicit ec: ExecutionContext)
  extends (scalaj.http.HttpRequest ⇒ Future[scalaj.http.HttpResponse[Array[Byte]]]) {
  import scala.concurrent.blocking

  override def apply(request: HttpRequest) = Future {
    blocking { request.asBytes }
  }
}
