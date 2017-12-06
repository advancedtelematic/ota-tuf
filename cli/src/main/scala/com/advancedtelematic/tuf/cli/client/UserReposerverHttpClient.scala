package com.advancedtelematic.tuf.cli.client

import java.net.URI

import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.libtuf.reposerver.{UserReposerverClient, UserReposerverHttpClient => UserReposerverHttpClientClass}
import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.tuf.cli.repo.TufRepo

import scala.concurrent.{ExecutionContext, Future}
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success}
import scalaj.http.HttpRequest

class ScalajHttpClient(implicit ec: ExecutionContext)
  extends (scalaj.http.HttpRequest ⇒ Future[scalaj.http.HttpResponse[Array[Byte]]]) {
  import scala.concurrent.blocking

  override def apply(request: HttpRequest) = Future {
    blocking { request.asBytes }
  }
}

object UserReposerverHttpClient {
  def apply(reposerverUri: URI, token: Option[String])(implicit ec: ExecutionContext): UserReposerverHttpClientClass =
    new UserReposerverHttpClientClass(reposerverUri, new ScalajHttpClient, token)

  def forRepo(repo: TufRepo)
             (implicit ec: ExecutionContext): Future[UserReposerverClient] =
    repo.authConfig match {
      case Success(Some(ac)) =>
        for {
          token <- AuthPlusClient.tokenFor(ac)
          repoUri <- repo.repoServerUri.toFuture
        } yield UserReposerverHttpClient(repoUri, Option(token.value))
      case Success(None) =>
        repo.repoServerUri.map { repoUri => UserReposerverHttpClient(repoUri, token = None) }.toFuture
      case Failure(ex) => Future.failed(ex)
    }
}
