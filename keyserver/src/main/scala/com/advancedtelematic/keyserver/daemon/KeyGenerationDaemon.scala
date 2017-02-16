package com.advancedtelematic.keyserver.daemon

import java.security.Security

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import com.advancedtelematic.keyserver.vault.VaultClient
import com.advancedtelematic.keyserver.{Settings, VersionInfo}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import com.advancedtelematic.libats.db.{BootMigrations, DatabaseConfig}
import com.advancedtelematic.libats.http.{BootApp, HealthResource}
import com.advancedtelematic.libats.monitoring.{DatabaseMetrics, MetricsSupport}

object KeyGenerationDaemon extends BootApp
    with Settings
    with VersionInfo
    with BootMigrations
    with DatabaseConfig
    with MetricsSupport
    with DatabaseMetrics {

    import com.advancedtelematic.libats.http.LogDirectives._
    import com.advancedtelematic.libats.http.VersionDirectives._

    implicit val _db = db

    Security.addProvider(new BouncyCastleProvider())

    log.info("Starting key gen daemon")

    val vaultClient = VaultClient(vaultAddr, vaultToken, vaultMount)

    val deviceSeenListener = system.actorOf(KeyGeneratorLeader.props(vaultClient), "keygen-leader")

    val routes: Route = (versionHeaders(version) & logResponseMetrics(projectName)) {
      new HealthResource(db, versionMap).route
    }

    Http().bindAndHandle(routes, host, port)
}

