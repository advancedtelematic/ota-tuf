package com.advancedtelematic.tuf.cli.client

import akka.http.scaladsl.model.Uri.Path
import com.advancedtelematic.libtuf.http.SHttpjServiceClient
import com.advancedtelematic.tuf.cli.DataType.{AuthConfig, AuthPlusToken}
import io.circe.Decoder

import scala.concurrent.{ExecutionContext, Future}


object AuthPlusClient {
  def apply(conf: AuthConfig)(implicit ec: ExecutionContext): AuthPlusClient =
    new AuthPlusClient(conf, new ScalajHttpClient)

  def tokenFor(conf: AuthConfig)(implicit ec: ExecutionContext): Future[AuthPlusToken] =
    apply(conf).authToken()
}


protected class AuthPlusClient(val config: AuthConfig,
                               httpClient: scalaj.http.HttpRequest => Future[scalaj.http.HttpResponse[Array[Byte]]])
                              (implicit ec: ExecutionContext)
  extends SHttpjServiceClient(httpClient) {

  private def apiUri(path: Path): String = config.server.withPath(path).toString()

  private val tokenResponseDecoder =
    Decoder.decodeString.prepare(_.downField("access_token")).map(AuthPlusToken.apply)

  def authToken(): Future[AuthPlusToken] = {

    val req = scalaj.http.Http(apiUri(Path("/token")))
      .auth(config.client_id, config.client_secret)
      .postForm(Seq("grant_type" → "client_credentials"))

    implicit val _decoder = tokenResponseDecoder
    execHttp[AuthPlusToken](req)()
  }
}
