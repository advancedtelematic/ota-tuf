package com.advancedtelematic.tuf.keyserver.daemon

import akka.actor.Scheduler

import java.security.Security
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import com.advancedtelematic.tuf.keyserver.{Settings, VersionInfo}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import com.advancedtelematic.libats.slick.db.{BootMigrations, DatabaseConfig, SlickEncryptionConfig}
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.slick.monitoring.{DatabaseMetrics, DbHealthResource}
import com.advancedtelematic.metrics.MetricsSupport
import com.advancedtelematic.metrics.prometheus.PrometheusMetricsSupport

object KeyGenerationDaemon extends BootApp
  with Settings
  with VersionInfo
  with BootMigrations
  with DatabaseConfig
  with MetricsSupport
  with DatabaseMetrics
  with PrometheusMetricsSupport
  with SlickEncryptionConfig {

  import com.advancedtelematic.libats.http.LogDirectives._
  import com.advancedtelematic.libats.http.VersionDirectives._
  import akka.http.scaladsl.server.Directives._

  implicit val _db = db
  implicit val scheduler: Scheduler = system.scheduler

  Security.addProvider(new BouncyCastleProvider())

  log.info("Starting key gen daemon")

  system.actorOf(KeyGeneratorLeader.props(), "keygen-leader")

  val routes: Route = (versionHeaders(version) & logResponseMetrics(projectName)) {
    DbHealthResource(versionMap).route ~ prometheusMetricsRoutes
  }

  Http().bindAndHandle(routes, host, port)
}
