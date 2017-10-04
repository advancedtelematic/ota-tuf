package com.advancedtelematic.tuf.cli.client

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.stream.Materializer
import com.advancedtelematic.libtuf.http.{ServiceHttpClient, ServiceHttpClientSupport}
import com.advancedtelematic.tuf.cli.DataType.{AuthConfig, AuthPlusToken}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.Decoder

import scala.concurrent.Future

object AuthPlusClient extends ServiceHttpClientSupport {
  def apply(conf: AuthConfig)
           (implicit system: ActorSystem, mat: Materializer): AuthPlusClient =
    new AuthPlusClient(conf, defaultHttpClient)

  def tokenFor(conf: AuthConfig)(implicit system: ActorSystem, mat: Materializer): Future[AuthPlusToken] =
    apply(conf).authToken()
}


protected class AuthPlusClient(val config: AuthConfig, httpClient: HttpRequest => Future[HttpResponse])
                              (implicit system: ActorSystem, mat: Materializer)
  extends ServiceHttpClient(httpClient) {

  private def apiUri(path: Path) = config.server.withPath(path)

  private val tokenResponseDecoder =
    Decoder.decodeString.prepare(_.downField("access_token")).map(AuthPlusToken.apply)

  def authToken(): Future[AuthPlusToken] = {
    val authHeaders = Authorization(BasicHttpCredentials(config.client_id, config.client_secret))
    val entity = FormData("grant_type" -> "client_credentials").toEntity

    val req = HttpRequest(HttpMethods.POST,
      uri = apiUri(Path("/token")), entity = entity).withHeaders(authHeaders)

    implicit val _decoder = tokenResponseDecoder
    execHttp[AuthPlusToken](req)()
  }
}
