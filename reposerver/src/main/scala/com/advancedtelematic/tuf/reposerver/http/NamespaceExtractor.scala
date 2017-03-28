package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.server.{AuthorizationFailedRejection, Directive1, Directives}
import com.advancedtelematic.libats.data.Namespace
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.reposerver.db.RepoNamespaceRepositorySupport
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext
import slick.driver.MySQLDriver.api._

abstract class NamespaceValidation(implicit val ec: ExecutionContext, val db: Database)
  extends RepoNamespaceRepositorySupport with Directives {

  def apply(repoId: RepoId): Directive1[Namespace]
}

object NamespaceExtractor {
  private val _log = LoggerFactory.getLogger(this.getClass)

  def default(implicit ec: ExecutionContext, db: Database): NamespaceValidation = new NamespaceValidation {
    def apply(repoId: RepoId): Directive1[Namespace] = {
      optionalHeaderValueByName("x-ats-namespace").flatMap {
        case Some(namespace) =>
          onSuccess(repoNamespaceRepo.belongsTo(repoId, Namespace(namespace))).flatMap {
            case true =>
              provide(Namespace(namespace))
            case false =>
              _log.info(s"User not allowed for ($repoId, $namespace)")
              reject(AuthorizationFailedRejection)
          }
        case None =>
          _log.info(s"User not allowed, no namespace provided for $repoId")
          reject(AuthorizationFailedRejection)
      }
    }
  }
}
