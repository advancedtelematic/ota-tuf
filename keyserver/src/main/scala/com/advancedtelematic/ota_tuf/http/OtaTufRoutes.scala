package com.advancedtelematic.ota_tuf.http

import akka.http.scaladsl.server.{Directives, _}
import akka.stream.Materializer
import com.advancedtelematic.libtuf.repo_store.RoleKeyStoreClient
import com.advancedtelematic.ota_tuf.VersionInfo
import com.advancedtelematic.ota_tuf.vault.VaultClient
import org.genivi.sota.http.{ErrorHandler, HealthResource}
import org.genivi.sota.rest.SotaRejectionHandler._

import scala.concurrent.ExecutionContext
import slick.driver.MySQLDriver.api._


class OtaTufRoutes(vaultClient: VaultClient, roleKeyStore: RoleKeyStoreClient)
                  (implicit val db: Database, val ec: ExecutionContext, mat: Materializer) extends VersionInfo {

  import Directives._

  val routes: Route =
    handleRejections(rejectionHandler) {
      ErrorHandler.handleErrors {
        pathPrefix("api" / "v1") {
            new RootRoleResource(vaultClient).route ~
            new RepoResource(roleKeyStore).route
        } ~ new HealthResource(db, versionMap).route
      }
    }
}
