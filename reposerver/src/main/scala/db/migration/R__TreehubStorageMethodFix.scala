package db.migration

import java.security.Security

import slick.jdbc.MySQLProfile.api._
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import com.advancedtelematic.libats.slick.db.AppMigration
import com.advancedtelematic.tuf.reposerver.db.TreehubStorageMethodFix
import org.bouncycastle.jce.provider.BouncyCastleProvider


class R__TreehubStorageMethodFix extends AppMigration  {
  Security.addProvider(new BouncyCastleProvider)

  implicit val system = ActorSystem(this.getClass.getSimpleName)
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  override def migrate(implicit db: Database) = new TreehubStorageMethodFix().run.map(_ => ())
}
