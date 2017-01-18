package com.advancedtelematic.ota_tuf.http

import akka.http.scaladsl.model.StatusCodes
import akka.stream.Materializer
import com.advancedtelematic.ota_tuf.data.DataType._
import com.advancedtelematic.ota_tuf.db.KeyGenRequestSupport
import com.advancedtelematic.ota_tuf.vault.VaultClient
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.ota_tuf.data.Codecs._
import com.advancedtelematic.ota_tuf.data.RoleType
import com.advancedtelematic.ota_tuf.roles.RootRoleGeneration
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import io.circe.{Decoder, Encoder, Json}

import scala.concurrent.ExecutionContext


class RootRoleResource(vaultClient: VaultClient)
                      (implicit val db: Database, val ec: ExecutionContext, mat: Materializer)
  extends KeyGenRequestSupport {
  import akka.http.scaladsl.server.Directives._
  import ClientRootGenRequest._

  val rootRoleGeneration = new RootRoleGeneration(vaultClient)
  val roleSigning = new RoleSigning(vaultClient)

  val route =
    pathPrefix("root" / GroupId.Path) { groupId =>
      pathEnd {
        (post & entity(as[ClientRootGenRequest])) { (genRequest: ClientRootGenRequest) =>
          require(genRequest.threshold == 1, "threshold > 1 not supported")

          val f = rootRoleGeneration
            .createDefaultGenRequest(groupId, genRequest.threshold)
            .map(StatusCodes.Accepted -> _)

          complete(f)
        } ~
          get {
            val f = rootRoleGeneration.findSigned(groupId)
            complete(f)
          }
      } ~
      path(RoleType.Path) { roleType =>
        (post & entity(as[Json])) { payload =>
          val f = roleSigning.signFor(groupId, roleType, payload)
          complete(f)
        }
      }
    }
}

object ClientRootGenRequest {
  implicit val encoder: Encoder[ClientRootGenRequest] = io.circe.generic.semiauto.deriveEncoder
  implicit val decoder: Decoder[ClientRootGenRequest] = io.circe.generic.semiauto.deriveDecoder
}

case class ClientRootGenRequest(threshold: Int = 1)
