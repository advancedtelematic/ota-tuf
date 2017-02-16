package com.advancedtelematic.keyserver

import java.security.Security

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.{Directives, Route}
import akka.stream.Materializer
import com.advancedtelematic.libtuf.repo_store.RoleKeyStoreHttpClient
import com.advancedtelematic.keyserver.http.OtaTufRoutes
import com.advancedtelematic.keyserver.vault.VaultClient
import com.advancedtelematic.libats.db.{BootMigrations, DatabaseConfig}
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.monitoring.{DatabaseMetrics, MetricsSupport}
import com.typesafe.config.ConfigFactory
import org.bouncycastle.jce.provider.BouncyCastleProvider
import com.advancedtelematic.libats.http.VersionDirectives._
import com.advancedtelematic.libats.http.LogDirectives._

trait Settings {
  lazy val config = ConfigFactory.load()

  val host = config.getString("server.host")
  val port = config.getInt("server.port")

  lazy val vaultAddr = Uri(config.getString("vault.address"))
  lazy val vaultToken = config.getString("vault.token")
  lazy val vaultMount = config.getString("vault.mount")
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

  lazy val vaultClient = VaultClient(vaultAddr, vaultToken, vaultMount)

  val localRoleKeyStore = Uri.from(scheme = "http", host = host, port = port, path = "/api/v1")

  lazy val keyStoreClient = new RoleKeyStoreHttpClient(localRoleKeyStore)

  val routes: Route =
    (versionHeaders(version) & logResponseMetrics(projectName)) {
      new OtaTufRoutes(vaultClient, keyStoreClient).routes
    }

  Http().bindAndHandle(routes, host, port)
}
