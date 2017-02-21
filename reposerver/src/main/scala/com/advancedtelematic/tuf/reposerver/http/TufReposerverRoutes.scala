package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.server.{Directives, _}
import akka.stream.Materializer
import com.advancedtelematic.libats.http.DefaultRejectionHandler._
import com.advancedtelematic.libats.http.{ErrorHandler, HealthResource}
import com.advancedtelematic.libtuf.repo_store.RoleKeyStoreClient
import com.advancedtelematic.tuf.reposerver.VersionInfo
import slick.driver.MySQLDriver.api._

import scala.concurrent.ExecutionContext


class TufReposerverRoutes(keyserverClient: RoleKeyStoreClient)
                         (implicit val db: Database, val ec: ExecutionContext, mat: Materializer) extends VersionInfo {

  import Directives._

  val routes: Route =
    handleRejections(rejectionHandler) {
      ErrorHandler.handleErrors {
        pathPrefix("api" / "v1") {
            new RepoResource(keyserverClient).route
        } ~ new HealthResource(db, versionMap).route
      }
    }
}
