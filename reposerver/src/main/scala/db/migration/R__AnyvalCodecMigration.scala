package db.migration

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.advancedtelematic.libats.slick.db.AppMigration
import com.advancedtelematic.libtuf.keyserver.KeyserverHttpClient
import com.advancedtelematic.tuf.reposerver.Settings
import com.advancedtelematic.tuf.reposerver.db.AnyvalCodecMigration
import slick.jdbc.MySQLProfile.api._

import scala.concurrent.Future

class R__AnyvalCodecMigration extends AppMigration with Settings {
  implicit val system = ActorSystem("R__AnyvalCodecMigration")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  override def migrate(implicit db: Database): Future[Unit] = {
    lazy val keyStoreClient = KeyserverHttpClient(keyServerUri)
    new AnyvalCodecMigration(keyStoreClient).run.map(_ => ())
  }
}
