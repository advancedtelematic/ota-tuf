package com.advancedtelematic.tuf.keyserver.http

import akka.http.scaladsl.model.StatusCodes
import akka.stream.Materializer
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, ValidKeyId}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.tuf.keyserver.roles.{RootRoleGeneration, RootRoleKeyEdit}
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import io.circe.{Decoder, Encoder, Json}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.tuf.keyserver.db.KeyGenRequestSupport

import scala.concurrent.ExecutionContext
import com.advancedtelematic.libats.data.RefinedUtils._
import com.advancedtelematic.libtuf.data.TufDataType._

class RootRoleResource(vaultClient: VaultClient)
                      (implicit val db: Database, val ec: ExecutionContext, mat: Materializer)
  extends KeyGenRequestSupport {
  import akka.http.scaladsl.server.Directives._
  import ClientRootGenRequest._

  val rootRoleGeneration = new RootRoleGeneration(vaultClient)
  val rootRoleKeyEdit = new RootRoleKeyEdit(vaultClient)
  val roleSigning = new RoleSigning(vaultClient)

  val KeyIdPath = Segment.flatMap(_.refineTry[ValidKeyId].toOption)

  val route =
    pathPrefix("root" / RepoId.Path) { repoId =>
      pathEnd {
        (post & entity(as[ClientRootGenRequest])) { (genRequest: ClientRootGenRequest) =>
          require(genRequest.threshold == 1, "threshold != 1 not supported")

          val f = rootRoleGeneration
            .createDefaultGenRequest(repoId, genRequest.threshold)
            .map(StatusCodes.Accepted -> _)

          complete(f)
        } ~
          get {
            val f = rootRoleGeneration.findAndCache(repoId)
            complete(f)
          }
      } ~
      pathPrefix("private_keys") {
        path(KeyIdPath) { keyId =>
          get {
            complete(rootRoleKeyEdit.fetchPrivateKey(repoId, keyId))
          } ~
          delete {
            val f = rootRoleGeneration
              .findAndCache(repoId)
              .flatMap(_ => rootRoleKeyEdit.deletePrivateKey(repoId, keyId))

            complete(f)
          }
        }
      } ~
      path(RoleType.Path) { roleType =>
        (post & entity(as[Json])) { payload =>
          val f = roleSigning.signFor(repoId, roleType, payload)
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
