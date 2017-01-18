package com.advancedtelematic.ota_tuf

import java.security.Security

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.{Directives, Route}
import akka.stream.Materializer
import com.advancedtelematic.ota_tuf.http.OtaTufRoutes
import com.advancedtelematic.ota_tuf.repo_store.RoleKeyStoreHttpClient
import com.advancedtelematic.ota_tuf.vault.VaultClient
import com.typesafe.config.ConfigFactory
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.genivi.sota.db.{BootMigrations, DatabaseConfig}
import org.genivi.sota.http.BootApp
import org.genivi.sota.http.LogDirectives.logResponseMetrics
import org.genivi.sota.http.VersionDirectives.versionHeaders
import org.genivi.sota.monitoring.{DatabaseMetrics, MetricsSupport}


trait Settings {
  lazy val config = ConfigFactory.load()

  val host = config.getString("server.host")
  val port = config.getInt("server.port")

  lazy val vaultAddr = Uri(config.getString("vault.address"))
  lazy val vaultToken = config.getString("vault.token")
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

  lazy val vaultClient = VaultClient(vaultAddr, vaultToken)

  val localRoleKeyStore = Uri.from(host = host, port = port, path = "/api/v1")

  lazy val keyStoreClient = new RoleKeyStoreHttpClient(localRoleKeyStore)

  val routes: Route =
    (versionHeaders(version) & logResponseMetrics(projectName)) {
      new OtaTufRoutes(vaultClient, keyStoreClient).routes
    }

  Http().bindAndHandle(routes, host, port)
}
