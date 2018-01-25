package db.migration

import java.security.Security

import akka.Done
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import com.advancedtelematic.libats.slick.codecs.SlickEnumMapper
import com.advancedtelematic.libats.slick.db.{AppMigration, SlickCirceMapper, SlickUUIDKey}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType}
import com.advancedtelematic.libtuf_server.db.SignatureMethodMigration
import com.advancedtelematic.libtuf_server.db.SignatureMethodMigration.Row
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepositorySupport
import org.bouncycastle.jce.provider.BouncyCastleProvider
import slick.jdbc.GetResult
import slick.jdbc.MySQLProfile.api._

import scala.concurrent.{ExecutionContext, Future}

class RepoServerSignatureMethodMigration(implicit
                                         val db: Database,
                                         val mat: Materializer,
                                         val system: ActorSystem,
                                         val ec: ExecutionContext
                                        ) extends SignedRoleRepositorySupport {

  implicit private val getRowResult: GetResult[Row] = GetResult { r =>
    val repoId = SlickUUIDKey.dbMapping[RepoId].getValue(r.rs, 1)
    val roleType = SlickEnumMapper.enumMapper(RoleType).getValue(r.rs, 2)
    val json = SlickCirceMapper.jsonMapper.getValue(r.rs, 3)

    Row(repoId, roleType, json)
  }

  def run: Future[Done] = {
    val query = sql"SELECT repo_id, role_type, content from signed_roles".as[Row]

    val replaceFn = (row: Row) => {
      signedRoleRepo.find(row.repoId, row.roleType).flatMap { old =>
        signedRoleRepo.persist(old, forceVersion = true).map(_ => ())
      }
    }

    new SignatureMethodMigration(query, replaceFn).run
  }
}



class R__01SignedRolesSigMethodMigration extends AppMigration  {
  Security.addProvider(new BouncyCastleProvider)

  implicit val system = ActorSystem("R__SignatureMethodMigration")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  override def migrate(implicit db: Database) = new RepoServerSignatureMethodMigration().run.map(_ => ())
}
