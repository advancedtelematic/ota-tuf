package com.advancedtelematic.tuf.reposerver.db

import java.time.Instant

import akka.{Done, NotUsed}
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libats.slick.codecs.SlickRefined
import com.advancedtelematic.libats.slick.db.{DatabaseConfig, SlickCirceMapper, SlickExtensions, SlickUUIDKey}
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.reposerver.Settings
import io.circe.Json
import slick.jdbc.GetResult
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libats.messaging_datatype.DataType._
import eu.timepit.refined.api.Refined

import scala.concurrent.{Await, ExecutionContext, Future}
import Schema._
import akka.actor.ActorSystem
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import com.advancedtelematic.libats.slick.codecs.SlickRefined._
import com.advancedtelematic.libats.slick.db.SlickUUIDKey._
import com.advancedtelematic.libtuf.data.ClientCodecs
import com.advancedtelematic.libtuf.data.TufSlickMappings._
import com.advancedtelematic.libtuf.keyserver.{KeyserverClient, KeyserverHttpClient}
import com.advancedtelematic.tuf.reposerver.http.SignedRoleGeneration
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration



object AnyvalCodecMigrationApp extends BootApp with DatabaseConfig with Settings {
  override lazy val projectName = "tuf-reposerver-migration"

  implicit val _db = db

  lazy val keyStoreClient = new KeyserverHttpClient(keyServerUri)

  Await.result(new AnyvalCodecMigration(keyStoreClient).run, Duration.Inf)

  log.info("Migration finished")

  system.terminate()
}


class AnyvalCodecMigration(keystoreClient: KeyserverClient)(implicit db: Database, ec: ExecutionContext, actorSystem: ActorSystem, materializer: ActorMaterializer) {
  val log = LoggerFactory.getLogger(this.getClass)

  case class Row(repoId: RepoId, filename: TargetFilename, json: Json, createdAt: Instant, updatedAt: Instant)

  val roleSigning = new SignedRoleGeneration(keystoreClient)

  val convertJsonFlow: Flow[Row, Row, NotUsed] = Flow[Row].mapAsyncUnordered(5)(convertJson)

  val regenerateIfOutdatedFlow: Flow[Row, Row, NotUsed] = Flow[Row].mapAsyncUnordered(3)(regenerateRolesIfOutdated)

  val sink: Sink[Row, Future[Done]] = Sink.foreach[Row] { case Row(repoId, filename, _, _, _) => log.info(s"Processed ($repoId, $filename)") }

  def run: Future[Done] = {
    val query = sql"SELECT repo_id, filename, custom, created_at, updated_at from target_items where custom is not null".as[Row]

    val source = db.stream(query)

    Source.fromPublisher(source)
      .via(convertJsonFlow)
      .via(regenerateIfOutdatedFlow)
      .runWith(sink)
  }

  def regenerateRolesIfOutdated(row: Row): Future[Row] = {
    val needsUpdate = db.run {
      sql"SELECT 1 from signed_roles sr join target_items ti using(repo_id) where ti.repo_id = '#${row.repoId.uuid.toString}' and ti.filename = '#${row.filename}' and ti.updated_at > sr.updated_at and sr.role_type = 'TARGETS' LIMIT 1".as[Int].headOption.map(_.isDefined)
    }

    needsUpdate.flatMap {
      case true =>
        log.info(s"signed role generation needed for (${row.repoId}, ${row.filename})")
        roleSigning
          .regenerateSignedRoles(row.repoId)
          .recover { case ex =>
            log.error(s"Could not regenerate signed roles for ${row.repoId}: ${ex.getMessage}")
          }.map(_ => row)
      case false =>
        FastFuture.successful(row)
    }
  }

  def convertJson(row: Row): Future[Row] = db.run {
    row.json.as[TargetCustom](ClientCodecs.legacyTargetCustomDecoder) match {
      case Left(_) =>
        log.info("No json reformat needed")
        DBIO.successful(row)
      case Right(old) =>
        log.info(s"Changing json for (${row.repoId}, ${row.filename})")
        targetItems
          .filter(_.repoId === row.repoId)
          .filter(_.filename === row.filename)
          .map(_.custom)
          .update(Option(old.copy(createdAt = row.createdAt, updatedAt = row.updatedAt)))
          .map(_ => row)
    }
  }

  implicit val getRowResult: GetResult[Row] = GetResult { r =>
    val repoId = SlickUUIDKey.dbMapping[RepoId].getValue(r.rs, 1)
    val filename: TargetFilename = SlickRefined.refinedMappedType[String, ValidTargetFilename, Refined].getValue(r.rs, 2)
    val json = SlickCirceMapper.jsonMapper.getValue(r.rs, 3)
    val createdAt = SlickExtensions.javaInstantMapping.getValue(r.rs, 4)
    val updatedAt = SlickExtensions.javaInstantMapping.getValue(r.rs, 5)

    Row(repoId, filename, json, createdAt, updatedAt)
  }
}
