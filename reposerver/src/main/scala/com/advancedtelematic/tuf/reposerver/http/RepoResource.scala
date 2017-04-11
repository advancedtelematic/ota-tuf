package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server._
import com.advancedtelematic.libats.data.Namespace
import com.advancedtelematic.libats.http.Errors.MissingEntity
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, RepoId, RoleType}
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, TargetItem}
import com.advancedtelematic.tuf.reposerver.db.{RepoNamespaceRepositorySupport, SignedRoleRepository, SignedRoleRepositorySupport, TargetItemRepositorySupport}
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import io.circe.syntax._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class RepoResource(roleKeyStore: KeyserverClient, namespaceValidation: NamespaceValidation)
                  (implicit val db: Database, val ec: ExecutionContext) extends Directives
  with TargetItemRepositorySupport with SignedRoleRepositorySupport with RepoNamespaceRepositorySupport {

  private val signedRoleGeneration = new SignedRoleGeneration(roleKeyStore)

  private val NamespaceHeader = headerValueByName("x-ats-namespace").map(Namespace)

  private def UserRepoId(namespace: Namespace): Directive1[RepoId] = Directive.Empty.tflatMap { _ =>
    onComplete(repoNamespaceRepo.findFor(namespace)).flatMap {
      case Success(repoId) => provide(repoId)
      case Failure(_: MissingEntity[_]) => reject(AuthorizationFailedRejection)
      case Failure(ex) => failWith(ex)
    }
  }

  private def createRepo(namespace: Namespace, repoId: RepoId): Route =
   complete {
     roleKeyStore
       .createRoot(repoId)
       .flatMap(_ => repoNamespaceRepo.persist(repoId, namespace))
       .map(_ => repoId)
   }

  private def addTarget(filename: String, repoId: RepoId, clientItem: RequestTargetItem): Route =
    complete {
      val item = TargetItem(repoId, filename, clientItem.uri, clientItem.checksum, clientItem.length)
      signedRoleGeneration.addToTarget(item)
    }

  private def findRole(repoId: RepoId, roleType: RoleType): Route = {
    complete {
      val signedRoleFut = roleType match {
        case RoleType.ROOT =>
          signedRoleRepo.find(repoId, roleType).recoverWith {
            case SignedRoleRepository.SignedRoleNotFound =>
              signedRoleGeneration.fetchRootRole(repoId)
          }
        case _ =>
          signedRoleRepo.find(repoId, roleType)
      }

      signedRoleFut.map(_.content)
    }
  }

  val route =
    (pathPrefix("user_repo") & NamespaceHeader) { namespace =>
      (post & pathEnd) {
        val repoId = RepoId.generate()
        createRepo(namespace, repoId)
      } ~
        UserRepoId(namespace) { repoId =>
          namespaceValidation(repoId) { _ =>
            (get & path(RoleType.JsonRoleTypeMetaPath)) { roleType =>
              findRole(repoId, roleType)
            } ~
            (post & path("targets" / Segment)) { filename =>
              entity(as[RequestTargetItem]) { clientItem =>
                addTarget(filename, repoId, clientItem)
              }
            }
          }
        }
    } ~
    pathPrefix("repo" / RepoId.Path) { repoId =>
      (pathEnd & post & NamespaceHeader) { namespace =>
        createRepo(namespace, repoId)
      } ~
      namespaceValidation(repoId) { _ =>
        path("targets" / Segment) { filename =>
          (post & entity(as[RequestTargetItem])) { clientItem =>
            addTarget(filename, repoId, clientItem)
          }
        } ~
        (get & path(RoleType.JsonRoleTypeMetaPath)) { roleType =>
          findRole(repoId, roleType)
        }
    }
  }
}

object RequestTargetItem {
  implicit val encoder: Encoder[RequestTargetItem] = deriveEncoder
  implicit val decoder: Decoder[RequestTargetItem] = deriveDecoder
}

case class RequestTargetItem(uri: Uri, checksum: Checksum, length: Long)
