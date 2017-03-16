package com.advancedtelematic.tuf.keyserver.roles

import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, SignedPayload}
import com.advancedtelematic.tuf.keyserver.db.{GeneratedRootRolesSupport, RootSignaturesSupport}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import scala.concurrent.{ExecutionContext, Future}

// TODO: Except this is not really a cache, we might not be able to generate keys again if they get deleted or the user just uploads a sign
class RootRoleStufSwchwarma(vaultClient: VaultClient)
                           (implicit val db: Database, val ec: ExecutionContext)
  extends GeneratedRootRolesSupport with RootSignaturesSupport {

  import scala.async.Async._

  val rootRoleGeneration = new RootRoleGeneration()
  val rootRoleSigning = new RootRoleSigning(vaultClient)

  def findSignedRoot(repoId: RepoId): Future[SignedPayload[RootRole]] = {
    // TODO: Fail with Locked if still fucked
    // TODO: Fail if not enough signatures

    generatedRootRoles.find(repoId).flatMap {
      case Some(role) => async {
        val sigs = await(rootSignaturesRepo.find(repoId)).map(_.toClient)
        SignedPayload(sigs, role)
      }
      case None => async {
        val rootRole = await(rootRoleGeneration.generate(repoId))
        val sigs = await(rootRoleSigning.persistSignaturesFromAvailableKeys(repoId, rootRole)).map(_.toClient)

        SignedPayload(sigs, rootRole)
      }
    }
  }
}
