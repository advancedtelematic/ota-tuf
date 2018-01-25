package db.migration

import java.security.Security

import slick.jdbc.MySQLProfile.api._
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import com.advancedtelematic.libats.slick.db.AppMigration
import com.advancedtelematic.tuf.reposerver.db.StorageMethodFix
import org.bouncycastle.jce.provider.BouncyCastleProvider


class R__02StorageMethodFix extends AppMigration  {
  Security.addProvider(new BouncyCastleProvider)

  implicit val system = ActorSystem(this.getClass.getSimpleName)
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  override def migrate(implicit db: Database) = new StorageMethodFix().run.map(_ => ())
}
