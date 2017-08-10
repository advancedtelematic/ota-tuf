package com.advancedtelematic.tuf.reposerver

import java.util.concurrent.TimeUnit

import cats.syntax.either._
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.monitoring.MetricsSupport
import com.advancedtelematic.metrics.{AkkaHttpMetricsSink, InfluxDbMetricsReporter, InfluxDbMetricsReporterSettings}
import com.typesafe.config.{ConfigException, ConfigFactory}
import scala.concurrent.duration.FiniteDuration

trait InfluxdbMetricsReporterSupport {
  self: BootApp with MetricsSupport =>

  private lazy val _config = ConfigFactory.load().getConfig("ats.metricsReporter")

  private lazy val metricsReporterSettings: Option[InfluxDbMetricsReporterSettings] = {
    val settings = for {
      reportEnabled <- Either.catchOnly[ConfigException](_config.getBoolean("reportMetrics"))
      maybeSettings <- if (reportEnabled)
        Either.catchOnly[ConfigException] {
          val host = _config.getString("host")
          val port = _config.getInt("port")
          val database = _config.getString("database")
          val serviceName = _config.getString("serviceName")
          val instanceId = _config.getString("instanceId")
          val interval = FiniteDuration(_config.getDuration("interval").toMillis, TimeUnit.MILLISECONDS)

          Option(InfluxDbMetricsReporterSettings(host, port, database, serviceName, instanceId, interval, None, None))
        }
      else {
        log.info("Metrics reporting disabled by config")
        Right(None)
      }
    } yield maybeSettings

    settings.valueOr { ex =>
      log.warn(s"Metrics reporting disabled: ${ex.getMessage}")
      None
    }
  }

  metricsReporterSettings.foreach { settings =>
    InfluxDbMetricsReporter.start(settings, metricRegistry, AkkaHttpMetricsSink.apply(settings))
  }
}
