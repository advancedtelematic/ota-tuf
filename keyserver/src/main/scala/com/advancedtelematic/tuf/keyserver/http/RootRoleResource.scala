package com.advancedtelematic.tuf.keyserver.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.stream.Materializer
import cats.data.Validated.{Invalid, Valid}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType}
import com.advancedtelematic.tuf.keyserver.roles.{RootRoleGeneration, RootRoleKeyEdit, TargetKeyAccess}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.tuf.keyserver.db.KeyGenRequestSupport
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.libtuf_server.data.Marshalling._
import com.advancedtelematic.libats.http.UUIDKeyPath.UUIDKeyPathOp
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import slick.jdbc.MySQLProfile.api._

class RootRoleResource(vaultClient: VaultClient)
                      (implicit val db: Database, val ec: ExecutionContext, mat: Materializer)
  extends KeyGenRequestSupport {
  import akka.http.scaladsl.server.Directives._
  import ClientRootGenRequest._

  val rootRoleGeneration = new RootRoleGeneration(vaultClient)
  val rootRoleKeyEdit = new RootRoleKeyEdit(vaultClient)
  val roleSigning = new RoleSigning(vaultClient)
  val targetRole = new TargetKeyAccess(vaultClient)

  val route =
    pathPrefix("root" / RepoId.Path) { repoId =>
      pathEnd {
        put {
          val f = rootRoleGeneration.forceRetry(repoId).map(_ => StatusCodes.OK)
          complete(f)
        } ~
        (post & entity(as[ClientRootGenRequest])) { (genRequest: ClientRootGenRequest) =>
          require(genRequest.threshold > 0, "threshold must be greater than 0")

          val f = rootRoleGeneration
            .createDefaultGenRequest(repoId, genRequest.threshold, genRequest.keyType)
            .map(StatusCodes.Accepted -> _)

          complete(f)
        } ~
        get {
          val f = rootRoleGeneration.findOrGenerate(repoId)
          complete(f)
        }
      } ~
      path(IntNumber) { version =>
        complete(rootRoleGeneration.findByVersion(repoId, version))
      } ~
      pathPrefix("private_keys") {
        path(KeyIdPath) { keyId =>
          delete {
            val f = rootRoleGeneration
              .findOrGenerate(repoId)
              .flatMap(_ => rootRoleKeyEdit.deletePrivateKey(repoId, keyId))
              .map(_ ⇒ StatusCodes.NoContent)

            complete(f)
          }
        }
      } ~
      path(RoleTypePath) { roleType =>
        (post & entity(as[Json])) { payload =>
          val f = roleSigning.signFor(repoId, roleType, payload)
          complete(f)
        }
      } ~
      path("unsigned") {
        get {
          val f = rootRoleGeneration.createUnsigned(repoId)
          complete(f)
        } ~
        (post & entity(as[SignedPayload[RootRole]])) { signedPayload =>
          val f: Future[ToResponseMarshallable] =
            rootRoleGeneration.storeUserSigned(repoId, signedPayload).map {
              case Valid(_) =>
                StatusCodes.NoContent
              case Invalid(errors) =>
                val obj = Json.obj("errors" -> errors.asJson)
                StatusCodes.BadRequest -> obj
            }
          complete(f)
        }
      } ~
      pathPrefix("keys") {
        (path(KeyIdPath) & get) { keyId ⇒
          complete(rootRoleKeyEdit.findKeyPair(repoId, keyId))
        } ~
        pathPrefix("targets") {
          (put & pathEnd){
            entity(as[TufKey]) { tufKey =>
              val f = rootRoleKeyEdit.addPublicKey(repoId, RoleType.TARGETS, tufKey)
              complete(f)
            }
          } ~
          path("pairs") {
            get {
              onComplete(targetRole.keyPairs(repoId)) {
                case Success(pairs) if pairs.nonEmpty => complete(pairs)
                case Success(_) => complete(StatusCodes.NotFound)
                case Failure(t) => complete(StatusCodes.InternalServerError -> t.getMessage)
              }
            }
          }
        }
      }
    }
}

object ClientRootGenRequest {
  implicit val encoder: Encoder[ClientRootGenRequest] = io.circe.generic.semiauto.deriveEncoder
  implicit val decoder: Decoder[ClientRootGenRequest] = io.circe.generic.semiauto.deriveDecoder
}

case class ClientRootGenRequest(threshold: Int = 1, keyType: KeyType = RsaKeyType)
