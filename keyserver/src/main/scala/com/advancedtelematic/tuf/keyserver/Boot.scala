package com.advancedtelematic.tuf.keyserver

import java.security.Security

import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{Directives, Route}
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.http.LogDirectives._
import com.advancedtelematic.libats.http.VersionDirectives._
import com.advancedtelematic.libats.http.monitoring.{BootMetrics, MetricsSupport}
import com.advancedtelematic.libats.http.tracing.Tracing
import com.advancedtelematic.libats.slick.db.{DatabaseConfig, SlickEncryptionConfig}
import com.advancedtelematic.libats.slick.monitoring.DatabaseMetrics
import com.advancedtelematic.metrics.prometheus.PrometheusMetricsSupport
import com.advancedtelematic.metrics.{AkkaHttpRequestMetrics, InfluxdbMetricsReporterSupport}
import com.advancedtelematic.tuf.keyserver.http.TufKeyserverRoutes
import com.typesafe.config.ConfigFactory
import org.bouncycastle.jce.provider.BouncyCastleProvider

trait Settings {
  private lazy val _config = ConfigFactory.load()

  val host = _config.getString("server.host")
  val port = _config.getInt("server.port")
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

  val tracing = Tracing.fromConfig(config, projectName)

  val routes: Route =
    (versionHeaders(version) & requestMetrics(metricRegistry) & logResponseMetrics(projectName) & logRequestResult(("tuf-keyserver", Logging.InfoLevel))) {
      tracing.traceRequests { _ =>
        new TufKeyserverRoutes(metricsRoutes = prometheusMetricsRoutes).routes
      }
    }

  Http().bindAndHandle(routes, host, port)
}
