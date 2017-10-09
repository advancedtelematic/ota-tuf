package com.advancedtelematic.tuf.cli.client

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libtuf.reposerver.{UserReposerverClient, UserReposerverHttpClient}
import com.advancedtelematic.tuf.cli.TufRepo

import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.cli.TryToFuture._
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

  def apply(reposerverUri: Uri, token: String)(implicit ec: ExecutionContext): UserReposerverHttpClient =
    new UserReposerverHttpClient(reposerverUri, new ScalajHttpClient, token)

  private def toTufUri(authPlusUri: Uri): Uri = {
    val host = authPlusUri.authority.host.toString()

    "^(.+?)-.+?\\.(.+)$".r.findFirstMatchIn(host) match {
      case Some(m) =>
        val env = m.group(1)
        val rest = m.group(2)

        authPlusUri.withHost(s"$env-tuf-reposerver-pub.$rest")
      case None =>
        val url = authPlusUri.withHost(host.replace("auth-plus", "tuf-reposerver-pub"))
        log.warn("Could not determine reposerver url from authplus url, using")
        url
    }
  }

  def forRepo(repo: TufRepo)
             (implicit ec: ExecutionContext): Future[UserReposerverClient] = for {
    authConfig <- repo.authConfig().toFuture
    token <- AuthPlusClient.tokenFor(authConfig)
  } yield UserReposerverHttpClient(toTufUri(authConfig.server), token.value)
}
