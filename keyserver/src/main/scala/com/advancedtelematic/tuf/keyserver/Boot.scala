package com.advancedtelematic.tuf.keyserver

import java.security.Security

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.{Directives, Route}
import com.advancedtelematic.tuf.keyserver.http.TufKeyserverRoutes
import com.advancedtelematic.libats.slick.db.DatabaseConfig
import com.advancedtelematic.libats.http.BootApp
import com.typesafe.config.ConfigFactory
import org.bouncycastle.jce.provider.BouncyCastleProvider
import com.advancedtelematic.libats.http.VersionDirectives._
import com.advancedtelematic.libats.http.LogDirectives._
import com.advancedtelematic.libats.http.monitoring.MetricsSupport
import com.advancedtelematic.libats.slick.monitoring.DatabaseMetrics
import com.advancedtelematic.metrics.InfluxdbMetricsReporterSupport
import com.advancedtelematic.tuf.keyserver.vault.VaultClient

import scala.concurrent.duration.Duration

trait Settings {
  private lazy val _config = ConfigFactory.load()

  val host = _config.getString("server.host")
  val port = _config.getInt("server.port")

  lazy val vaultAddr = Uri(_config.getString("vault.address"))
  lazy val vaultToken = _config.getString("vault.token")
  lazy val vaultMount = Path(_config.getString("vault.mount"))
  lazy val vaultRenewInterval = Duration.fromNanos(_config.getDuration("vault.renewInterval").toNanos)
}

object Boot extends BootApp
  with Directives
  with Settings
  with VersionInfo
  with DatabaseConfig
  with MetricsSupport
  with DatabaseMetrics
  with InfluxdbMetricsReporterSupport {

  implicit val _db = db

  Security.addProvider(new BouncyCastleProvider)

  log.info(s"Starting $version on http://$host:$port")

  lazy val vaultClient = VaultClient(vaultAddr, vaultToken, vaultMount)

  val routes: Route =
    (versionHeaders(version) & logResponseMetrics(projectName)) {
      new TufKeyserverRoutes(vaultClient).routes
    }

  Http().bindAndHandle(routes, host, port)
}
