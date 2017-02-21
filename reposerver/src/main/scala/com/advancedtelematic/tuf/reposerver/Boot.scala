package com.advancedtelematic.keyserver

import java.security.Security

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Path.Empty
import akka.http.scaladsl.server.{Directives, Route}
import akka.stream.Materializer
import com.advancedtelematic.libtuf.repo_store.RoleKeyStoreHttpClient
import com.advancedtelematic.libats.db.{BootMigrations, DatabaseConfig}
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.monitoring.{DatabaseMetrics, MetricsSupport}
import com.typesafe.config.ConfigFactory
import org.bouncycastle.jce.provider.BouncyCastleProvider
import com.advancedtelematic.libats.http.VersionDirectives._
import com.advancedtelematic.libats.http.LogDirectives._
import com.advancedtelematic.tuf.reposerver.VersionInfo
import com.advancedtelematic.tuf.reposerver.http.TufReposerverRoutes

trait Settings {
  lazy val config = ConfigFactory.load()

  val host = config.getString("server.host")
  val port = config.getInt("server.port")

  val keyServerUri = Uri(config.getString("keyserver.uri"))
}

object Boot extends BootApp
  with Directives
  with Settings
  with VersionInfo
  with DatabaseConfig
  with BootMigrations
  with MetricsSupport
  with DatabaseMetrics {

  implicit val _db = db

  Security.addProvider(new BouncyCastleProvider())

  log.info(s"Starting $version on http://$host:$port")

  lazy val keyStoreClient = new RoleKeyStoreHttpClient(keyServerUri.withPath(Empty / "api/v1"))

  val routes: Route =
    (versionHeaders(version) & logResponseMetrics(projectName)) {
      new TufReposerverRoutes(keyStoreClient).routes
    }

  Http().bindAndHandle(routes, host, port)
}
