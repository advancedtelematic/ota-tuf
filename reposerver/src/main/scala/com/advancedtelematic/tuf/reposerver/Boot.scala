package com.advancedtelematic.tuf.reposerver

import java.security.Security

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.{Directives, Route}
import com.advancedtelematic.libtuf.keyserver.KeyserverHttpClient
import com.advancedtelematic.libats.slick.db.{BootMigrations, DatabaseConfig}
import cats.syntax.either._
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.monitoring.MetricsSupport
import com.typesafe.config.ConfigFactory
import org.bouncycastle.jce.provider.BouncyCastleProvider
import com.advancedtelematic.libats.http.VersionDirectives._
import com.advancedtelematic.libats.http.LogDirectives._
import com.advancedtelematic.libats.slick.monitoring.DatabaseMetrics
import com.advancedtelematic.libats.messaging.MessageBus
import com.advancedtelematic.tuf.reposerver.http.NamespaceExtractor
import com.advancedtelematic.tuf.reposerver.http.TufReposerverRoutes
import com.advancedtelematic.tuf.reposerver.target_store.{LocalTargetStore, S3Credentials, S3TargetStore}
import com.amazonaws.regions.Regions
import net.i2p.crypto.eddsa.EdDSASecurityProvider

trait Settings {
  lazy val config = ConfigFactory.load()

  lazy val host = config.getString("server.host")
  lazy val port = config.getInt("server.port")

  lazy val keyServerUri = Uri(config.getString("keyserver.uri"))

  lazy val targetStoreRoot = config.getString("storage.localStorageRoot")

  lazy val s3Credentials = {
    val accessKey = config.getString("storage.s3.accessKey")
    val secretKey = config.getString("storage.s3.secretKey")
    val bucketId = config.getString("storage.s3.bucketId")
    val region = Regions.fromName(config.getString("storage.s3.region"))
    new S3Credentials(accessKey, secretKey, bucketId, region)
  }

  lazy val useS3 = config.getString("storage.type").equals("s3")
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

  Security.addProvider(new BouncyCastleProvider)
  Security.addProvider(new EdDSASecurityProvider)

  log.info(s"Starting $version on http://$host:$port")

  lazy val keyStoreClient = new KeyserverHttpClient(keyServerUri)

  val messageBusPublisher = MessageBus.publisher(system, config).valueOr(throw _)

  val targetStore = if(useS3) new S3TargetStore(s3Credentials) else LocalTargetStore(targetStoreRoot)

  val routes: Route =
    (versionHeaders(version) & logResponseMetrics(projectName)) {
      new TufReposerverRoutes(keyStoreClient, NamespaceExtractor.default, targetStore, messageBusPublisher).routes
    }

  Http().bindAndHandle(routes, host, port)
}
