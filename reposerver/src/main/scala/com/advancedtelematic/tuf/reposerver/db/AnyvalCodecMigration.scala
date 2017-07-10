package com.advancedtelematic.tuf.reposerver.db

import akka.{Done, NotUsed}
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libats.slick.codecs.SlickRefined
import com.advancedtelematic.libats.slick.db.{DatabaseConfig, SlickCirceMapper, SlickUUIDKey}
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.reposerver.Settings
import io.circe.Json
import slick.jdbc.GetResult
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libats.messaging_datatype.DataType._
import eu.timepit.refined.api.Refined

import scala.concurrent.{ExecutionContext, Future}
import Schema._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.advancedtelematic.libats.slick.codecs.SlickRefined._
import com.advancedtelematic.libats.slick.db.SlickUUIDKey._
import com.advancedtelematic.libtuf.data.ClientCodecs
import com.advancedtelematic.libtuf.data.TufSlickMappings._
import org.slf4j.LoggerFactory



object AnyvalCodecMigrationApp extends BootApp with DatabaseConfig with Settings {
  override lazy val projectName = "tuf-reposerver-migration"

  implicit val _db = db

  (new AnyvalCodecMigration).run
}


class AnyvalCodecMigration(implicit db: Database, ec: ExecutionContext, actorSystem: ActorSystem, materializer: ActorMaterializer) {
  implicit val log = LoggerFactory.getLogger(this.getClass)

  type Row = (RepoId, TargetFilename, Json)

  val flow: Flow[Row, Row, NotUsed] = Flow[Row].mapAsyncUnordered(5)(convert)

  val sink: Sink[Row, Future[Done]] = Sink.foreach[Row] { case (repoId, filename, _) => log.info(s"Processed ($repoId, $filename)") }

  def run: Future[Done] = {
    val query = sql"SELECT repo_id, filename, custom from target_items where custom is not null".as[Row]

    val source = db.stream(query)

    Source.fromPublisher(source).via(flow).runWith(sink)
  }

  def convert(row: Row): Future[Row] = row match { case (repoId, filename, json) =>
    val dbio =
      json.as[TargetCustom](ClientCodecs.legacyTargetCustomDecoder) match {
        case Left(_) =>
          log.info(s"No change required for ($repoId, $filename)")
          DBIO.successful(row)
        case Right(old) =>
          log.info(s"Changing json for ($repoId, $filename)")
          targetItems
            .filter(_.repoId === repoId)
            .filter(_.filename === filename)
            .map(_.custom)
            .update(Option(old))
            .map(_ => row)
      }

    db.run(dbio)
  }

  implicit val getRowResult: GetResult[Row] = GetResult { r =>
    val repoId = SlickUUIDKey.dbMapping[RepoId].getValue(r.rs, 1)
    val filename: TargetFilename = SlickRefined.refinedMappedType[String, ValidTargetFilename, Refined].getValue(r.rs, 2)
    val json = SlickCirceMapper.jsonMapper.getValue(r.rs, 3)

    (repoId, filename, json)
  }
}
