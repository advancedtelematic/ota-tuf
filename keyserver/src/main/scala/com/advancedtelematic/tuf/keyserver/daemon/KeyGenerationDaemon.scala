package com.advancedtelematic.tuf.keyserver.daemon

import java.security.Security

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import com.advancedtelematic.tuf.keyserver.{Settings, VersionInfo}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import com.advancedtelematic.libats.slick.db.{BootMigrations, DatabaseConfig, SlickEncryptionConfig}
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.http.monitoring.MetricsSupport
import com.advancedtelematic.libats.slick.monitoring.{DatabaseMetrics, DbHealthResource}

object KeyGenerationDaemon extends BootApp
  with Settings
  with VersionInfo
  with BootMigrations
  with DatabaseConfig
  with MetricsSupport
  with DatabaseMetrics
  with SlickEncryptionConfig {

  import com.advancedtelematic.libats.http.LogDirectives._
  import com.advancedtelematic.libats.http.VersionDirectives._

  implicit val _db = db

  Security.addProvider(new BouncyCastleProvider())

  log.info("Starting key gen daemon")

  system.actorOf(KeyGeneratorLeader.props(), "keygen-leader")

  val routes: Route = (versionHeaders(version) & logResponseMetrics(projectName)) {
    DbHealthResource(versionMap).route
  }

  Http().bindAndHandle(routes, host, port)
}
