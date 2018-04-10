package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.server.{Directives, _}
import akka.stream.Materializer
import com.advancedtelematic.libats.http.DefaultRejectionHandler._
import com.advancedtelematic.libats.http.{ErrorHandler, HealthCheck}
import com.advancedtelematic.libats.http.monitoring.{ServiceHealthCheck, VaultHealthCheck}
import com.advancedtelematic.libats.slick.monitoring.DbHealthResource
import com.advancedtelematic.libats.messaging.MessageBusPublisher
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.VersionInfo
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore
import slick.jdbc.MySQLProfile.api._

import scala.concurrent.ExecutionContext


class TufReposerverRoutes(keyserverClient: KeyserverClient,
                          namespaceValidation: NamespaceValidation,
                          targetStore: TargetStore,
                          messageBusPublisher: MessageBusPublisher,
                          dependencyChecks: Seq[HealthCheck] = Seq.empty)
                         (implicit val db: Database, val ec: ExecutionContext, mat: Materializer) extends VersionInfo {

  import Directives._

  val routes: Route =
    handleRejections(rejectionHandler) {
      ErrorHandler.handleErrors {
        pathPrefix("api" / "v1") {
          new RepoResource(keyserverClient, namespaceValidation, targetStore, new TufTargetsPublisher(messageBusPublisher)).route
        } ~ DbHealthResource(versionMap, dependencies = dependencyChecks).route
      }
    }
}
