package db.migration

import java.sql.Timestamp
import java.time.Instant

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.advancedtelematic.libats.slick.db.{AppMigration, SlickCirceMapper, SlickUUIDKey}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType}
import com.advancedtelematic.tuf.reposerver.Settings
import io.circe.Json
import slick.jdbc.GetResult
import slick.jdbc.MySQLProfile.api._
import cats.syntax.either._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}

class ExpireAtPopulateMigration()
                               (implicit db: Database, ec: ExecutionContext, actorSystem: ActorSystem, materializer: ActorMaterializer) {
  import com.advancedtelematic.libats.codecs.AkkaCirce._

  case class Row(repoId: RepoId, roleType: RoleType, json: Json)

  private val log = LoggerFactory.getLogger(this.getClass)

  val populateFlow = Flow[Row].mapAsyncUnordered(4) { row =>
    val expire = row.json
      .hcursor
      .downField("signed")
      .downField("expires")
      .as[Instant]
      .map(Timestamp.from)

    expire match {
      case Left(_) =>
        log.warn(s"could not parse expire date for (${row.repoId}, ${row.roleType})")
        FastFuture.successful(row)
      case Right(instant) =>
        val updateQuery = sqlu"UPDATE signed_roles set expires_at = $instant where repo_id = ${row.repoId.uuid.toString} and role_type = ${row.roleType.toString}"
        db.run(updateQuery).map(_ => row)
    }
  }

  def run: Future[Done] = {
    val query = sql"SELECT repo_id, role_type, content from signed_roles WHERE expires_at is NULL".as[Row]

    Source.fromPublisher(db.stream(query))
      .via(populateFlow)
      .runWith(Sink.foreach { row =>
        log.info(s"Processed (${row.repoId}, ${row.roleType})")
      })
  }

  implicit val getRowResult: GetResult[Row] = GetResult { r =>
    val repoId = SlickUUIDKey.dbMapping[RepoId].getValue(r.rs, 1)
    val roleType = RoleType.enumMapper.getValue(r.rs, 2)
    val json = SlickCirceMapper.jsonMapper.getValue(r.rs, 3)

    Row(repoId, roleType, json)
  }
}


class R__ExpireAtPopulateMigration  extends AppMigration with Settings {
  implicit val system = ActorSystem(this.getClass.getSimpleName)
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  override def migrate(implicit db: Database) = {
    new ExpireAtPopulateMigration().run.map(_ => ())
  }
}
