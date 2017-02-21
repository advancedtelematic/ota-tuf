package com.advancedtelematic.tuf.keyserver.http

import akka.http.scaladsl.server.{Directives, _}
import akka.stream.Materializer
import com.advancedtelematic.libtuf.repo_store.RoleKeyStoreClient
import com.advancedtelematic.tuf.keyserver.VersionInfo
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.libats.http.{ErrorHandler, HealthResource}
import com.advancedtelematic.libats.http.DefaultRejectionHandler._

import scala.concurrent.ExecutionContext
import slick.driver.MySQLDriver.api._


class TufKeyserverRoutes(vaultClient: VaultClient)
                        (implicit val db: Database, val ec: ExecutionContext, mat: Materializer) extends VersionInfo {

  import Directives._

  val routes: Route =
    handleRejections(rejectionHandler) {
      ErrorHandler.handleErrors {
        pathPrefix("api" / "v1") {
            new RootRoleResource(vaultClient).route
        } ~ new HealthResource(db, versionMap).route
      }
    }
}
