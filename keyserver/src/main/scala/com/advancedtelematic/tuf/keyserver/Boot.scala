package com.advancedtelematic.tuf.keyserver

import java.security.Security

import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.{Directives, Route}
import com.advancedtelematic.tuf.keyserver.http.TufKeyserverRoutes
import com.advancedtelematic.libats.slick.db.{DatabaseConfig, SlickEncryptionConfig}
import com.advancedtelematic.libats.http.BootApp
import com.typesafe.config.ConfigFactory
import org.bouncycastle.jce.provider.BouncyCastleProvider
import com.advancedtelematic.libats.http.VersionDirectives._
import com.advancedtelematic.libats.http.LogDirectives._
import com.advancedtelematic.libats.http.monitoring.{BootMetrics, MetricsSupport}
import com.advancedtelematic.libats.slick.monitoring.DatabaseMetrics
import com.advancedtelematic.metrics.{AkkaHttpRequestMetrics, InfluxdbMetricsReporterSupport}
import com.advancedtelematic.metrics.prometheus.PrometheusMetricsSupport

trait Settings {
  private lazy val _config = ConfigFactory.load()

  val host = _config.getString("server.host")
  val port = _config.getInt("server.port")

  lazy val vaultAddr = Uri(_config.getString("vault.address"))
  lazy val vaultToken = _config.getString("vault.token")
  lazy val vaultMount = Path(_config.getString("vault.mount"))
}


object Boot extends BootApp
  with Directives
  with Settings
  with VersionInfo
  with DatabaseConfig
  with MetricsSupport
  with BootMetrics
  with DatabaseMetrics
  with SlickEncryptionConfig
  with InfluxdbMetricsReporterSupport
  with AkkaHttpRequestMetrics
  with PrometheusMetricsSupport {

  implicit val _db = db

  Security.addProvider(new BouncyCastleProvider)

  log.info(s"Starting $version on http://$host:$port")

  val routes: Route =
    (versionHeaders(version) & requestMetrics(metricRegistry) & logResponseMetrics(projectName) & logRequestResult(("tuf-keyserver", Logging.DebugLevel))) {
      new TufKeyserverRoutes(metricsRoutes = prometheusMetricsRoutes).routes
    }

  Http().bindAndHandle(routes, host, port)
}
