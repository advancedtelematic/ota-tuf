package db.migration

import java.security.Security

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.advancedtelematic.libats.slick.db.AppMigration
import com.advancedtelematic.tuf.keyserver.Settings
import com.advancedtelematic.tuf.keyserver.db.MovePrivateKeysFromVaultMigration
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import org.bouncycastle.jce.provider.BouncyCastleProvider
import slick.jdbc.MySQLProfile.api._

class R__01MovePrivateKeysFromVault extends AppMigration with Settings {

  Security.addProvider(new BouncyCastleProvider)

  implicit lazy val system = ActorSystem("R__01MovePrivateKeysFromVault")
  implicit lazy val materializer = ActorMaterializer()
  import system.dispatcher

  lazy val vaultClient = VaultClient(vaultAddr, vaultToken, vaultMount)

  override def migrate(implicit db: Database) =
    new MovePrivateKeysFromVaultMigration(vaultClient).migrate.map(_ => ())
}
