package com.advancedtelematic.tuf.cli.http

import java.net.URI

import com.advancedtelematic.libtuf.http._
import com.advancedtelematic.tuf.cli.repo.TufRepo
import com.advancedtelematic.tuf.cli.TryToFuture._
import org.slf4j

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object TufRepoCliClient {
  private val log = slf4j.LoggerFactory.getLogger(this.getClass)

  trait HttpClientBuilder[S <: TufServerClient] {
    def apply(reposerverUri: URI, token: Option[String])(implicit ec: ExecutionContext): S
  }

  implicit val reposerverHttpClientBuilder = new HttpClientBuilder[ReposerverClient] {
    override def apply(reposerverUri: URI, token: Option[String])(implicit ec: ExecutionContext): ReposerverClient =
      new ReposerverHttpClient(reposerverUri, new ScalajHttpClient, token)
  }

  implicit val directorHttpClientBuilder = new HttpClientBuilder[DirectorClient] {
    override def apply(reposerverUri: URI, token: Option[String])(implicit ec: ExecutionContext): DirectorClient =
      new DirectorHttpClient(reposerverUri, new ScalajHttpClient, token)
  }

  def forRepo[S <: TufServerClient](repo: TufRepo[S])
                                   (implicit ec: ExecutionContext, httpClientBuilder: HttpClientBuilder[S]): Future[S] =
    repo.authConfig match {
      case Success(Some(ac)) =>
        for {
          token <- AuthPlusClient.tokenFor(ac)
          _ = log.debug(s"client token: ${token.value}")
          repoUri <- repo.repoServerUri.toFuture
        } yield httpClientBuilder.apply(repoUri, Option(token.value))
      case Success(None) =>
        repo.repoServerUri.map { repoUri => httpClientBuilder.apply(repoUri, None) }.toFuture
      case Failure(ex) => Future.failed(ex)
    }
}
