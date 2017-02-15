package com.advancedtelematic.ota_tuf.daemon

import java.security.Security

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import com.advancedtelematic.ota_tuf.vault.VaultClient
import com.advancedtelematic.ota_tuf.{Settings, VersionInfo}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.genivi.sota.db.{BootMigrations, DatabaseConfig}
import org.genivi.sota.http.{BootApp, HealthResource}
import org.genivi.sota.monitoring.{DatabaseMetrics, MetricsSupport}

object KeyGenerationDaemon extends BootApp
    with Settings
    with VersionInfo
    with BootMigrations
    with DatabaseConfig
    with MetricsSupport
    with DatabaseMetrics {

    import org.genivi.sota.http.LogDirectives._
    import org.genivi.sota.http.VersionDirectives._

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

