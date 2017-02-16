package com.advancedtelematic.keyserver.http

import akka.http.scaladsl.server.{Directives, _}
import akka.stream.Materializer
import com.advancedtelematic.libtuf.repo_store.RoleKeyStoreClient
import com.advancedtelematic.keyserver.VersionInfo
import com.advancedtelematic.keyserver.vault.VaultClient
import com.advancedtelematic.libats.http.{ErrorHandler, HealthResource}
import com.advancedtelematic.libats.http.DefaultRejectionHandler._

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
