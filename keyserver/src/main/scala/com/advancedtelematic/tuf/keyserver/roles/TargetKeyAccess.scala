package com.advancedtelematic.tuf.keyserver.roles

import cats.implicits._
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, TufKeyPair}
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.MySQLProfile.api._


class TargetKeyAccess(vaultClient: VaultClient)
                     (implicit val db: Database, val ec: ExecutionContext)
  extends KeyGenRequestSupport with KeyRepositorySupport {

  def keyPairs(repoId: RepoId): Future[Seq[TufKeyPair]] =
    keyRepo.repoKeysForRole(repoId, RoleType.TARGETS).flatMap { keys =>
      keys.toList.traverse { key =>
        vaultClient.findKey(key.id).flatMap { vaultKeyPair =>
          Future.fromTry(vaultKeyPair.toTufKeyPair)
        }
      }
    }

}
