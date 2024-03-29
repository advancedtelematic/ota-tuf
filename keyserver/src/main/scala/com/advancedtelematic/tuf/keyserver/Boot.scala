package com.advancedtelematic.tuf.keyserver

import akka.actor.Scheduler

import java.security.Security
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{Directives, Route}
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.http.LogDirectives._
import com.advancedtelematic.libats.http.VersionDirectives._
import com.advancedtelematic.libats.http.tracing.Tracing
import com.advancedtelematic.libats.slick.db.{CheckMigrations, DatabaseConfig, SlickEncryptionConfig}
import com.advancedtelematic.libats.slick.monitoring.DatabaseMetrics
import com.advancedtelematic.metrics.prometheus.PrometheusMetricsSupport
import com.advancedtelematic.metrics.{AkkaHttpConnectionMetrics, AkkaHttpRequestMetrics, MetricsSupport}
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
  with DatabaseMetrics
  with CheckMigrations
  with SlickEncryptionConfig
  with AkkaHttpRequestMetrics
  with AkkaHttpConnectionMetrics
  with PrometheusMetricsSupport {

  implicit val _db = db
  implicit val scheduler: Scheduler = system.scheduler

  Security.addProvider(new BouncyCastleProvider)

  log.info(s"Starting $version on http://$host:$port")

  val tracing = Tracing.fromConfig(config, projectName)

  val routes: Route =
    (versionHeaders(version) & requestMetrics(metricRegistry) & logResponseMetrics(projectName) & logRequestResult(("tuf-keyserver", Logging.DebugLevel))) {
      tracing.traceRequests { _ =>
        new TufKeyserverRoutes(metricsRoutes = prometheusMetricsRoutes).routes
      }
    }

  Http().bindAndHandle(withConnectionMetrics(routes, metricRegistry), host, port)
}
