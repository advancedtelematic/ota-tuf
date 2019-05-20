package com.advancedtelematic.tuf.cli.http

import java.net.URI

import com.advancedtelematic.libtuf.http._
import com.advancedtelematic.tuf.cli.DataType.{MutualTlsConfig, OAuthConfig}
import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.tuf.cli.repo.TufRepo
import org.slf4j
import scalaj.http.HttpRequest

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object TufRepoCliClient {
  private val log = slf4j.LoggerFactory.getLogger(this.getClass)

  trait HttpClientBuilder[S <: TufServerClient] {
    def apply(reposerverUri: URI, auth: HttpRequest => HttpRequest)(implicit ec: ExecutionContext): S
  }

  implicit val reposerverHttpClientBuilder = new HttpClientBuilder[ReposerverClient] {
    override def apply(reposerverUri: URI, auth: HttpRequest => HttpRequest)(implicit ec: ExecutionContext): ReposerverClient =
      new ReposerverHttpClient(reposerverUri, auth.andThen(new ScalajHttpClient))
  }

  implicit val directorHttpClientBuilder = new HttpClientBuilder[DirectorClient] {
    override def apply(reposerverUri: URI, auth: HttpRequest => HttpRequest)(implicit ec: ExecutionContext): DirectorClient =
      new DirectorHttpClient(reposerverUri, auth.andThen(new ScalajHttpClient))
  }

  def forRepo[S <: TufServerClient](repo: TufRepo[S])
                                   (implicit ec: ExecutionContext, httpClientBuilder: HttpClientBuilder[S]): Future[S] =
    repo.authConfig match {
      case Success(Some(ac: OAuthConfig)) =>
        for {
          token <- AuthPlusClient.tokenFor(ac)
          _ = log.debug(s"client token: ${token.value}")
          repoUri <- repo.repoServerUri.toFuture
        } yield httpClientBuilder.apply(repoUri, Auth.oauth(token.value))

      case Success(Some(tls: MutualTlsConfig)) =>
        repo.repoServerUri.toFuture.map { uri =>
          val clientCertPath = repo.repoPath.resolve(tls.certPath)
          val serverCertPath = tls.serverCertPath.map(repo.repoPath.resolve)

          httpClientBuilder.apply(uri, Auth.mutualTls(clientCertPath, serverCertPath))
        }

      case Success(None) =>
        repo.repoServerUri.map { repoUri => httpClientBuilder.apply(repoUri, Auth.none) }.toFuture

      case Failure(ex) => Future.failed(ex)
    }
}
