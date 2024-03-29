package com.advancedtelematic.tuf.reposerver

import akka.actor.Scheduler

import java.security.Security
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.{Directives, Route}
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.http.LogDirectives._
import com.advancedtelematic.libats.http.VersionDirectives._
import com.advancedtelematic.libats.http.monitoring.ServiceHealthCheck
import com.advancedtelematic.libats.http.tracing.Tracing
import com.advancedtelematic.libats.http.tracing.Tracing.ServerRequestTracing
import com.advancedtelematic.libats.messaging.MessageBus
import com.advancedtelematic.libats.slick.db.{BootMigrations, DatabaseConfig}
import com.advancedtelematic.libats.slick.monitoring.DatabaseMetrics
import com.advancedtelematic.libtuf_server.keyserver.KeyserverHttpClient
import com.advancedtelematic.metrics.prometheus.PrometheusMetricsSupport
import com.advancedtelematic.metrics.{AkkaHttpRequestMetrics, MetricsSupport}
import com.advancedtelematic.tuf.reposerver
import com.advancedtelematic.tuf.reposerver.http.{NamespaceValidation, TufReposerverRoutes}
import com.advancedtelematic.tuf.reposerver.target_store._
import com.amazonaws.regions.Regions
import com.typesafe.config.ConfigFactory
import org.bouncycastle.jce.provider.BouncyCastleProvider

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.duration


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

  lazy val azureSettings = {
    val azureConfig = _config.getConfig("storage.azure")
    val connectionString = azureConfig.getString("connectionString")
    val signatureTtl = FiniteDuration.apply(azureConfig.getDuration("signatureTtl").getSeconds, duration.SECONDS)
    reposerver.target_store.AzureTargetStoreEngine.BlobStorageSettings(connectionString, signatureTtl)
  }

  lazy val outOfBandUploadLimit = _config.getBytes("storage.outOfBandUploadLimit")

  lazy val useS3 = _config.getString("storage.type").equals("s3")

  lazy val useAzure = _config.getString("storage.type").equals("azure")

  lazy val multipartUploadPartSize = _config.getBytes("storage.multipart.partSize")

  lazy val userRepoSizeLimit = _config.getLong("reposerver.sizeLimit")

  // not using Config.getDuration() here because that parses different formats than what Akka uses
  lazy val userRepoUploadRequestTimeout = Duration(_config.getString("reposerver.uploadRequestTimeout"))

  lazy val targetsFileSizeLimit = _config.getInt("reposerver.targetsFileSizeLimit")
}

object Boot extends BootApp
  with Directives
  with Settings
  with VersionInfo
  with DatabaseConfig
  with BootMigrations
  with MetricsSupport
  with DatabaseMetrics
  with AkkaHttpRequestMetrics
  with PrometheusMetricsSupport {

  implicit val _db = db
  implicit val scheduler: Scheduler = system.scheduler

  Security.addProvider(new BouncyCastleProvider)

  log.info(s"Starting $version on http://$host:$port")

  def keyStoreClient(implicit requestTracing: ServerRequestTracing) = KeyserverHttpClient(keyServerUri)

  val messageBusPublisher = MessageBus.publisher(system, config)

  val targetStoreEngine = if(useS3) {
    new S3TargetStoreEngine(s3Credentials)
  } else if (useAzure) {
    new AzureTargetStoreEngine(azureSettings)
  } else {
    LocalTargetStoreEngine(targetStoreRoot)
  }

  def targetStore(implicit requestTracing: ServerRequestTracing) = TargetStore(keyStoreClient,  targetStoreEngine, messageBusPublisher)

  val keyserverHealthCheck = new ServiceHealthCheck(keyServerUri)

  implicit val tracing = Tracing.fromConfig(config, "reposerver")

  val routes: Route =
    (versionHeaders(version) & requestMetrics(metricRegistry) & logResponseMetrics(projectName) & logRequestResult(("reposerver", Logging.DebugLevel))) {
      tracing.traceRequests { implicit requestTracing =>
        new TufReposerverRoutes(keyStoreClient, NamespaceValidation.withDatabase, targetStore,
          messageBusPublisher,
          prometheusMetricsRoutes,
          Seq(keyserverHealthCheck)).routes
      }
    }

  Http().bindAndHandle(routes, host, port)
}
