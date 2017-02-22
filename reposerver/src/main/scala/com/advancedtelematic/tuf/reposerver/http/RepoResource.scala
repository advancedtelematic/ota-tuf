package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.{AuthorizationFailedRejection, Directive1, Directives}
import com.advancedtelematic.libats.data.Namespace
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, RepoId, RoleType}
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.TargetItem
import com.advancedtelematic.tuf.reposerver.db.{RepoNamespaceRepositorySupport, SignedRoleRepositorySupport, TargetItemRepositorySupport}
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import scala.concurrent.ExecutionContext

class RepoResource(roleKeyStore: KeyserverClient, namespaceValidation: NamespaceValidation)
                  (implicit val db: Database, val ec: ExecutionContext) extends Directives
  with TargetItemRepositorySupport with SignedRoleRepositorySupport with RepoNamespaceRepositorySupport {

  val signedRoleGeneration = new SignedRoleGeneration(roleKeyStore)

  val NamespaceHeader = headerValueByName("x-ats-namespace").map(Namespace)

  val route =
    pathPrefix("repo" / RepoId.Path) { repoId =>
      (pathEnd & post & NamespaceHeader) { namespace =>
        val f = roleKeyStore.createRoot(repoId).flatMap(_ => repoNamespaceRepo.persist(repoId, namespace))
        complete(f)
      } ~
      namespaceValidation(repoId) { _ =>
        path("targets" / Segment) { filename =>
          (post & entity(as[RequestTargetItem])) { clientItem =>
            val item = TargetItem(repoId, filename, clientItem.uri, clientItem.checksum, clientItem.length)
            val f = signedRoleGeneration.addToTarget(item)
            complete(f)
          }
        } ~
        path(RoleType.JsonRoleTypeMetaPath) { roleType =>
          get {
            val f = signedRoleRepo
              .find(repoId, roleType)
              .map(_.content)

            complete(f)
          }
        }
    }
  }
}

object RequestTargetItem {
  implicit val encoder: Encoder[RequestTargetItem] = deriveEncoder
  implicit val decoder: Decoder[RequestTargetItem] = deriveDecoder
}

case class RequestTargetItem(uri: Uri, checksum: Checksum, length: Long)
