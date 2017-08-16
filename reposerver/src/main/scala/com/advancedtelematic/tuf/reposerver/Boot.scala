package com.advancedtelematic.tuf.reposerver

import java.security.Security
import java.util.concurrent.TimeUnit

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
import com.advancedtelematic.metrics.InfluxdbMetricsReporterSupport
import com.advancedtelematic.tuf.reposerver.http.{NamespaceValidation, TufReposerverRoutes}
import com.advancedtelematic.tuf.reposerver.target_store.{LocalTargetStore, S3Credentials, S3TargetStore}
import com.advancedtelematic.tuf.reposerver.target_store.TargetUpload
import com.amazonaws.regions.Regions

trait Settings {
  private lazy val _config = ConfigFactory.load()

  lazy val host = _config.getString("server.host")
  lazy val port = _config.getInt("server.port")

  lazy val keyServerUri = Uri(_config.getString("keyserver.uri"))

  lazy val targetStoreRoot = _config.getString("storage.localStorageRoot")

  lazy val s3Credentials = {
    val accessKey = _config.getString("storage.s3.accessKey")
    val secretKey = _config.getString("storage.s3.secretKey")
    val bucketId = _config.getString("storage.s3.bucketId")
    val region = Regions.fromName(_config.getString("storage.s3.region"))
    new S3Credentials(accessKey, secretKey, bucketId, region)
  }

  lazy val useS3 = _config.getString("storage.type").equals("s3")

  lazy val userRepoSizeLimit = _config.getInt("reposerver.sizeLimit")
}

object Boot extends BootApp
  with Directives
  with Settings
  with VersionInfo
  with DatabaseConfig
  with BootMigrations
  with MetricsSupport
  with DatabaseMetrics
  with InfluxdbMetricsReporterSupport {

  implicit val _db = db

  Security.addProvider(new BouncyCastleProvider)

  log.info(s"Starting $version on http://$host:$port")

  lazy val keyStoreClient = KeyserverHttpClient(keyServerUri)

  val messageBusPublisher = MessageBus.publisher(system, config).valueOr(throw _)

  val targetStore = if(useS3) new S3TargetStore(s3Credentials) else LocalTargetStore(targetStoreRoot)

  val targetUpload = new TargetUpload(
    keyStoreClient,
    targetStore,
    req => Http.get(system).singleRequest(req),
    messageBusPublisher)

  val routes: Route =
    (versionHeaders(version) & logResponseMetrics(projectName)) {
      new TufReposerverRoutes(keyStoreClient, NamespaceValidation.withDatabase, targetUpload, messageBusPublisher).routes
    }

  Http().bindAndHandle(routes, host, port)
}
