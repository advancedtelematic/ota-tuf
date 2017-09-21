package com.advancedtelematic.tuf.cli.client

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.stream.Materializer
import com.advancedtelematic.libtuf.http.ServiceHttpClientSupport
import com.advancedtelematic.libtuf.reposerver.{UserReposerverClient, UserReposerverHttpClient}
import com.advancedtelematic.tuf.cli.TufRepo

import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.cli.TryToFuture._
import org.slf4j.LoggerFactory

object UserReposerverHtttpClient extends ServiceHttpClientSupport {
  private val log = LoggerFactory.getLogger(this.getClass)

  def apply(reposerverUri: Uri, token: String)
           (implicit ec: ExecutionContext, system: ActorSystem, mat: Materializer): UserReposerverHttpClient =
    new UserReposerverHttpClient(reposerverUri, defaultHttpClient, token)

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
             (implicit system: ActorSystem, mat: Materializer, ec: ExecutionContext): Future[UserReposerverClient] = for {
    authConfig <- repo.authConfig().toFuture
    token <- AuthPlusClient.tokenFor(authConfig)
  } yield UserReposerverHtttpClient(toTufUri(authConfig.server), token.value)
}
