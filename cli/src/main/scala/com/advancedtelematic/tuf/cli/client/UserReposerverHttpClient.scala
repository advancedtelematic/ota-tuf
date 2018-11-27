package com.advancedtelematic.tuf.cli.client

import java.net.URI

import com.advancedtelematic.libtuf.reposerver.{UserDirectorHttpClient => UserDirectorHttpClientClass, UserReposerverHttpClient => UserReposerverHttpClientClass}
import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.tuf.cli.repo.TufRepo
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait UserHttpClient {
  type UserHttpClientType

  lazy private val log = LoggerFactory.getLogger(this.getClass)

  def apply(reposerverUri: URI, token: Option[String])(implicit ec: ExecutionContext): UserHttpClientType

  def forRepo(repo: TufRepo)(implicit ec: ExecutionContext): Future[UserHttpClientType] =
    repo.authConfig match {
      case Success(Some(ac)) =>
        for {
          token <- AuthPlusClient.tokenFor(ac)
          _ = log.debug(s"client token: ${token.value}")
          repoUri <- repo.repoServerUri.toFuture
        } yield apply(repoUri, Option(token.value))
      case Success(None) =>
        repo.repoServerUri.map { repoUri => apply(repoUri, token = None) }.toFuture
      case Failure(ex) => Future.failed(ex)
    }
}

object UserReposerverHttpClient extends UserHttpClient {
  type UserHttpClientType = UserReposerverHttpClientClass

  override def apply(reposerverUri: URI, token: Option[String])(implicit ec: ExecutionContext): UserReposerverHttpClientClass =
    new UserReposerverHttpClientClass(reposerverUri, new ScalajHttpClient, token)
}

object UserDirectorHttpClient extends UserHttpClient {
  type UserHttpClientType = UserDirectorHttpClientClass

  override def apply(reposerverUri: URI, token: Option[String])(implicit ec: ExecutionContext): UserDirectorHttpClientClass =
    new UserDirectorHttpClientClass(reposerverUri, new ScalajHttpClient, token)
}
