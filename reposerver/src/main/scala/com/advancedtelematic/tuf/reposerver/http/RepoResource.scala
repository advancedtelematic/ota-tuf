package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.unmarshalling._
import PredefinedFromStringUnmarshallers.CsvSeq
import com.advancedtelematic.libats.data.RefinedUtils._
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server._
import com.advancedtelematic.libats.data.Namespace
import com.advancedtelematic.libats.http.Errors.MissingEntity
import com.advancedtelematic.libats.messaging.MessageBusPublisher
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, HardwareIdentifier, RepoId, RoleType}
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.TargetItem
import com.advancedtelematic.tuf.reposerver.db.{RepoNamespaceRepositorySupport, SignedRoleRepository, SignedRoleRepositorySupport, TargetItemRepositorySupport}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libats.messaging_datatype.DataType.{TargetFilename, ValidTargetFilename}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.reposerver.target_store.{TargetStore, TargetUpload}
import io.circe.syntax._
import com.advancedtelematic.libats.http.RefinedMarshallingSupport
import com.advancedtelematic.libtuf.data.TufDataType._
import RefinedMarshallingSupport._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import com.advancedtelematic.libats.http.AnyvalMarshallingSupport._
import com.advancedtelematic.libats.codecs.AkkaCirce._

class RepoResource(roleKeyStore: KeyserverClient, namespaceValidation: NamespaceValidation,
                   targetStore: TargetStore, messageBusPublisher: MessageBusPublisher)
                  (implicit val db: Database, val ec: ExecutionContext) extends Directives
  with TargetItemRepositorySupport with SignedRoleRepositorySupport with RepoNamespaceRepositorySupport {

  private val signedRoleGeneration = new SignedRoleGeneration(roleKeyStore)
  private val targetUpload = new TargetUpload(roleKeyStore, targetStore, messageBusPublisher)
  private val NamespaceHeader = headerValueByName("x-ats-namespace").map(Namespace)

  private val TargetFilenamePath = Segments.flatMap {
    _.mkString("/").refineTry[ValidTargetFilename].toOption
  }

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

  private def addTarget(filename: TargetFilename, repoId: RepoId, clientItem: RequestTargetItem): Route =
    complete {
      val custom = for {
        name <- clientItem.name
        version <- clientItem.version
      } yield TargetCustom(name, version, clientItem.hardwareIds)

      val item = TargetItem(repoId, filename, clientItem.uri, clientItem.checksum, clientItem.length, custom)
      signedRoleGeneration.addToTarget(item)
    }


  private def addTargetFromContent(filename: TargetFilename, repoId: RepoId): Route = {
    parameters('name.as[TargetName], 'version.as[TargetVersion], 'hardwareIds.as(CsvSeq[HardwareIdentifier])) { (name, version, hardwareIdentifiers) =>
      fileUpload("file") { case (_, file) =>
        val custom = TargetCustom(name, version, hardwareIdentifiers)
        val action = targetUpload.store(repoId, filename, file, custom)
        complete(action)
      }
    }
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

  private def modifyRepoRoutes(repoId: RepoId)  =
    namespaceValidation(repoId) { _ =>
      (get & path(RoleType.JsonRoleTypeMetaPath)) { roleType =>
        findRole(repoId, roleType)
      } ~
      (post & path("targets" / TargetFilenamePath)) { filename =>
        entity(as[RequestTargetItem]) { clientItem =>
          addTarget(filename, repoId, clientItem)
        }
      } ~
      (put & path("targets" / TargetFilenamePath)) { filename =>
        addTargetFromContent(filename, repoId)
      } ~
      (get & path("targets" / TargetFilenamePath)) { filename =>
        complete(targetUpload.retrieve(repoId, filename))
      }
    }

  val route =
    (pathPrefix("user_repo") & NamespaceHeader) { namespace =>
      (post & pathEnd) {
        val repoId = RepoId.generate()
        createRepo(namespace, repoId)
      } ~
      UserRepoId(namespace) { repoId =>
        modifyRepoRoutes(repoId)
      }
    } ~
    pathPrefix("repo" / RepoId.Path) { repoId =>
      (pathEnd & post & NamespaceHeader) { namespace =>
        createRepo(namespace, repoId)
      } ~
        modifyRepoRoutes(repoId)
    }
}

object RequestTargetItem {
  implicit val encoder: Encoder[RequestTargetItem] = deriveEncoder
  implicit val decoder: Decoder[RequestTargetItem] = deriveDecoder
}

case class RequestTargetItem(uri: Uri, checksum: Checksum,
                             name: Option[TargetName],
                             version: Option[TargetVersion],
                             hardwareIds: Seq[HardwareIdentifier], length: Long)
