package db.migration


import akka.actor.ActorSystem
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink}
import akka.Done
import com.advancedtelematic.libats.slick.db.AppMigration
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientHashes, MetaItem, MetaPath}
import com.advancedtelematic.libtuf.data.TufDataType.{RoleType, SignedPayload}
import com.advancedtelematic.libtuf.keyserver.{KeyserverClient, KeyserverHttpClient}
import com.advancedtelematic.tuf.reposerver.Settings
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.SignedRole
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepositorySupport
import io.circe.{Decoder, Json}
import org.slf4j.LoggerFactory
import slick.jdbc.MySQLProfile.api._
import io.circe.generic.semiauto._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.codecs.AkkaCirce._
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.libats.messaging_datatype.MessageCodecs._

class TimestampCodecMigration(keystoreClient: KeyserverClient)
                             (implicit val db: Database, val ec: ExecutionContext, actorSystem: ActorSystem, materializer: ActorMaterializer)
  extends SignedRoleRepositorySupport {

  val log = LoggerFactory.getLogger(this.getClass)

  import com.advancedtelematic.libtuf.data.RefinedStringEncoding._

  case class LegacyMetaItem(version: Option[Int], hashes: ClientHashes, length: Long)

  implicit val legacyDecoder: Decoder[LegacyMetaItem] = deriveDecoder

  def run: Future[Done] = {
    signedRoleRepo.findAll(RoleType.TIMESTAMP, RoleType.SNAPSHOT)
      .via(Flow[SignedRole].mapAsyncUnordered(5)(processRow))
      .runWith {
        Sink.foreach[SignedRole] { role =>
          log.info(s"Processed (${role.repoId}, ${role.roleType})")
        }
      }
  }

  def convertMeta(old: SignedPayload[Json]): Json = {
    import cats.syntax.either._
    import io.circe.syntax._

    val oldMeta = old.signed.hcursor.downField("meta").as[Map[MetaPath, LegacyMetaItem]].valueOr(throw _)

    val newMeta = oldMeta.mapValues { m =>
      MetaItem(m.hashes, m.length, m.version.getOrElse(1))
    }

    old.signed.hcursor.downField("meta").withFocus(_ => newMeta.asJson).top.get
  }

  def processRow(row: SignedRole): Future[SignedRole] = {
    val meta =
      row.content
        .signed
        .hcursor
        .downField("meta")
        .as[Map[MetaPath, MetaItem]]

    meta match {
      case Right(_) =>
        log.info(s"No json reformat needed for ${row.repoId}, ${row.roleType}")
        FastFuture.successful(row)
      case Left(_) =>
        log.info(s"Changing json for (${row.repoId}, ${row.roleType})")

        val newPayload = convertMeta(row.content)

        keystoreClient.sign(row.repoId, row.roleType, newPayload).flatMap { signedPayload =>
          val newSignedRole = SignedRole.withChecksum(row.repoId, row.roleType, signedPayload, row.version, row.expireAt)
          signedRoleRepo.update(newSignedRole).map(_ => newSignedRole)
        }
    }
  }
}


class R__TimestampCodecMigration extends AppMigration with Settings {
  implicit val system = ActorSystem(this.getClass.getSimpleName)
  implicit val mat = ActorMaterializer()
  import system.dispatcher

  override def migrate(implicit db: Database): Future[Unit] = {
    lazy val keyStoreClient = KeyserverHttpClient(keyServerUri)
    new TimestampCodecMigration(keyStoreClient).run.map(_ => ())
  }
}
