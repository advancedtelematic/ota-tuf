package com.advancedtelematic.ota_tuf.http

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Directives
import com.advancedtelematic.ota_tuf.data.DataType.RepoId
import com.advancedtelematic.ota_tuf.db.{SignedRoleRepositorySupport, TargetItemRepositorySupport}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import com.advancedtelematic.ota_tuf.data.Codecs._

import scala.concurrent.ExecutionContext
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.{Checksum, TargetItem}
import com.advancedtelematic.ota_tuf.data.RoleType
import com.advancedtelematic.ota_tuf.repo_store.RoleKeyStoreClient
import com.advancedtelematic.ota_tuf.roles.SignedRoleGeneration

class RepoResource(roleKeyStore: RoleKeyStoreClient)
                  (implicit val db: Database, val ec: ExecutionContext) extends Directives
  with TargetItemRepositorySupport with SignedRoleRepositorySupport {

  val signedRoleGeneration = new SignedRoleGeneration(roleKeyStore)

  val route =
    pathPrefix("repo" / RepoId.Path) { repoId =>
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

object RequestTargetItem {
  implicit val encoder: Encoder[RequestTargetItem] = deriveEncoder
  implicit val decoder: Decoder[RequestTargetItem] = deriveDecoder
}

case class RequestTargetItem(uri: Uri, checksum: Checksum, length: Long)
