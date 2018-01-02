package db.migration

import java.security.Security

import akka.Done
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, Materializer}
import com.advancedtelematic.libats.slick.codecs.SlickRefined
import com.advancedtelematic.libats.slick.db.AppMigration
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, ValidKeyId}
import com.advancedtelematic.tuf.keyserver.Settings
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import eu.timepit.refined.api.Refined
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.slf4j.LoggerFactory
import slick.jdbc.GetResult
import slick.jdbc.MySQLProfile.api._

import scala.concurrent.Future

class VaultKeyDecodersMigration(vaultClient: VaultClient)
                               (implicit db: Database, mat: Materializer, system: ActorSystem) {

  import system.dispatcher

  val _log = LoggerFactory.getLogger(this.getClass)

  implicit private val getRowResult: GetResult[KeyId] = slick.jdbc.GetResult { r =>
    SlickRefined.refinedMappedType[String, ValidKeyId, Refined].getValue(r.rs, 1)
  }

  def migrate: Future[Done] = {
    val query = sql"SELECT key_id from `keys`".as[KeyId]

    val source = Source.fromPublisher(db.stream(query)).mapAsync(3) { keyId =>
      val f = for {
        oldKey <- vaultClient.findKey(keyId)
        _ <- vaultClient.createKey(oldKey)
      } yield keyId

      f.recover {
        case ex =>
          _log.warn(s"Could not process $keyId", ex)
          keyId
      }
    }

    source.runWith(Sink.foreach(key => _log.info(s"Processed $key")))
  }
}

class R__VaultKeyDecodersMigration extends AppMigration with Settings {

  Security.addProvider(new BouncyCastleProvider)

  implicit val system = ActorSystem("R__VaultKeyDecodersMigration")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val vaultClient = VaultClient(vaultAddr, vaultToken, vaultMount)

  override def migrate(implicit db: Database): Future[Unit] = new VaultKeyDecodersMigration(vaultClient).migrate.map(_ => ())
}
