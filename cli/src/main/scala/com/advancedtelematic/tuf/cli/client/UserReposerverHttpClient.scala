package com.advancedtelematic.tuf.cli.client

import java.net.URI

import com.advancedtelematic.libtuf.reposerver.{UserReposerverClient, UserReposerverHttpClient => UserReposerverHttpClientClass}
import com.advancedtelematic.tuf.cli.TufRepo
import com.advancedtelematic.tuf.cli.TryToFuture._
import scala.concurrent.{ExecutionContext, Future}
import org.slf4j.LoggerFactory

import scalaj.http.HttpRequest

class ScalajHttpClient(implicit ec: ExecutionContext)
  extends (scalaj.http.HttpRequest ⇒ Future[scalaj.http.HttpResponse[Array[Byte]]]) {
  import scala.concurrent.blocking

  override def apply(request: HttpRequest) = Future {
    blocking { request.asBytes }
  }
}

object UserReposerverHttpClient {
  private val log = LoggerFactory.getLogger(this.getClass)

  def apply(reposerverUri: URI, token: String)(implicit ec: ExecutionContext): UserReposerverHttpClientClass =
    new UserReposerverHttpClientClass(reposerverUri, new ScalajHttpClient, token)

  private def toTufUri(authPlusUri: URI): URI = {
    val host = authPlusUri.getHost

    "^(.+?)-.+?\\.(.+)$".r.findFirstMatchIn(host) match {
      case Some(m) =>
        val env = m.group(1)
        val rest = m.group(2)

        new URI(authPlusUri.getScheme,
          authPlusUri.getRawUserInfo,
          s"$env-tuf-reposerver-pub.$rest",
          authPlusUri.getPath,
          authPlusUri.getFragment
        )
      case None =>
        val url =
          new URI(authPlusUri.getScheme,
            authPlusUri.getRawUserInfo,
            host.replace("auth-plus", "tuf-reposerver-pub"),
            authPlusUri.getPath,
            authPlusUri.getFragment)

        log.warn("Could not determine reposerver url from authplus url, using $url")

        url
    }
  }

  def forRepo(repo: TufRepo)
             (implicit ec: ExecutionContext): Future[UserReposerverClient] = for {
    authConfig <- repo.authConfig().toFuture
    token <- AuthPlusClient.tokenFor(authConfig)
  } yield UserReposerverHttpClient(toTufUri(authConfig.server), token.value)
}
