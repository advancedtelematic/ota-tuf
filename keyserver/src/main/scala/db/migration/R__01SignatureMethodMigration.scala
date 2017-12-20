package db.migration

import java.security.Security

import akka.Done
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import com.advancedtelematic.libats.slick.db.{AppMigration, SlickCirceMapper, SlickUUIDKey}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType}
import com.advancedtelematic.libtuf_server.db.SignatureMethodMigration
import com.advancedtelematic.libtuf_server.db.SignatureMethodMigration.Row
import com.advancedtelematic.tuf.keyserver.Settings
import com.advancedtelematic.tuf.keyserver.db.SignedRootRoleSupport
import org.bouncycastle.jce.provider.BouncyCastleProvider
import slick.jdbc
import slick.jdbc.GetResult
import slick.jdbc.MySQLProfile.api._

import scala.concurrent.{ExecutionContext, Future}

class KeyserverSignatureMethodMigration(implicit
                                        val db: Database,
                                        val mat: Materializer,
                                        val system: ActorSystem,
                                        val ec: ExecutionContext
                                       ) extends SignedRootRoleSupport {

  implicit private val getRowResult: GetResult[Row] = jdbc.GetResult { r =>
    val repoId = SlickUUIDKey.dbMapping[RepoId].getValue(r.rs, 1)
    val json = SlickCirceMapper.jsonMapper.getValue(r.rs, 2)

    Row(repoId, RoleType.ROOT, json)
  }


  def run: Future[Done] = {
    val query = sql"SELECT repo_id, signed_payload from signed_root_roles".as[Row]

    val replaceFn = (row: Row) => {
      signedRootRoleRepo.findLatestValid(row.repoId).flatMap { old =>
        signedRootRoleRepo.persist(row.repoId, old)
      }
    }

    new SignatureMethodMigration(query, replaceFn).run
  }
}


class R__01SignatureMethodMigration extends AppMigration with Settings {
  Security.addProvider(new BouncyCastleProvider)

  implicit val system = ActorSystem("R__SignatureMethodMigration")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  override def migrate(implicit db: Database) = new KeyserverSignatureMethodMigration().run.map(_ => ())
}

