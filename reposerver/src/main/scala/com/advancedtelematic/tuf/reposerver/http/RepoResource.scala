package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.unmarshalling._
import PredefinedFromStringUnmarshallers.CsvSeq
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{StatusCodes, Uri}
import akka.http.scaladsl.server._
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import com.advancedtelematic.libats.data.RefinedUtils._
import com.advancedtelematic.libats.http.Errors.MissingEntity
import com.advancedtelematic.libats.messaging.MessageBusPublisher
import com.advancedtelematic.libtuf_server.data.Messages.TufTargetAdded
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, RepoId, _}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType._
import com.advancedtelematic.tuf.reposerver.db.{RepoNamespaceRepositorySupport, TargetItemRepositorySupport}
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, RootRole, TargetCustom, TargetsRole}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore
import com.advancedtelematic.libats.http.RefinedMarshallingSupport._
import com.advancedtelematic.libats.http.AnyvalMarshallingSupport._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.codecs.CirceCodecs._
import com.advancedtelematic.libats.data.DataType.{Checksum, Namespace}
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient.RequestTargetItem
import com.advancedtelematic.libats.http.UUIDKeyPath._
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.Settings
import com.advancedtelematic.libtuf_server.data.Marshalling._
import com.advancedtelematic.tuf.reposerver.http.Errors.NoRepoForNamespace
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import org.slf4j.LoggerFactory
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import slick.jdbc.MySQLProfile.api._
import RoleChecksumHeader._


class TufTargetsPublisher(messageBus: MessageBusPublisher)(implicit ec: ExecutionContext) {
  def targetAdded(namespace: Namespace, item: TargetItem): Future[Unit] =
    messageBus.publish(TufTargetAdded(namespace, item.filename, item.checksum, item.length, item.custom))

  def newTargetsAdded(namespace: Namespace, allTargets: Map[TargetFilename, ClientTargetItem], existing: Seq[TargetItem]): Future[Unit] = {
    newTargetsFromExisting(allTargets, existing.map(_.filename)).toList.traverse_ { case (filename, checksum, clientTargetItem) =>
      messageBus.publish(TufTargetAdded(namespace, filename, checksum,
        clientTargetItem.length, clientTargetItem.customParsed[TargetCustom]))
    }
  }

  private def newTargetsFromExisting(allTargets: Map[TargetFilename, ClientTargetItem], existing: Seq[TargetFilename]) =
    (allTargets -- existing.toSet).flatMap { case (targetFilename, clientTargetItem) =>
      clientTargetItem.hashes.headOption.map { case (hashMethod, validChecksum) =>
        (targetFilename, Checksum(hashMethod, validChecksum), clientTargetItem)
      }
    }
}

object RepoResource {

  object CreateRepositoryRequest {
    implicit val encoder: Encoder[CreateRepositoryRequest] = io.circe.generic.semiauto.deriveEncoder
    implicit val decoder: Decoder[CreateRepositoryRequest] = io.circe.generic.semiauto.deriveDecoder
  }

  case class CreateRepositoryRequest(keyType: KeyType)
}

class RepoResource(keyserverClient: KeyserverClient, namespaceValidation: NamespaceValidation,
                   targetStore: TargetStore, tufTargetsPublisher: TufTargetsPublisher)
                  (implicit val db: Database, val ec: ExecutionContext) extends Directives
  with TargetItemRepositorySupport
  with RepoNamespaceRepositorySupport
  with Settings {

  import RepoResource.CreateRepositoryRequest
  import CreateRepositoryRequest._

  private val signedRoleGeneration = new SignedRoleGeneration(keyserverClient)
  private val offlineSignedRoleStorage = new OfflineSignedRoleStorage(keyserverClient)

  val log = LoggerFactory.getLogger(this.getClass)

  private val targetCustomParameters: Directive1[TargetCustom] =
    parameters(
      'name.as[TargetName],
      'version.as[TargetVersion],
      'hardwareIds.as(CsvSeq[HardwareIdentifier]).?(immutable.Seq.empty[HardwareIdentifier]),
      'targetFormat.as[TargetFormat].?
    ).tmap { case (name, version, hardwareIds, targetFormat) =>
      TargetCustom(name, version, hardwareIds, targetFormat)
    }

  private val TargetFilenamePath = Segments.flatMap {
    _.mkString("/").refineTry[ValidTargetFilename].toOption
  }

  private def UserRepoId(namespace: Namespace): Directive1[RepoId] = Directive.Empty.tflatMap { _ =>
    onComplete(repoNamespaceRepo.findFor(namespace)).flatMap {
      case Success(repoId) => provide(repoId)
      case Failure(_: MissingEntity[_]) => failWith(NoRepoForNamespace(namespace))
      case Failure(ex) => failWith(ex)
    }
  }

  private def createRepo(namespace: Namespace, repoId: RepoId, keyType: KeyType): Route =
   complete {
     repoNamespaceRepo.ensureNotExists(namespace)
       .flatMap(_ => keyserverClient.createRoot(repoId, keyType))
       .flatMap(_ => repoNamespaceRepo.persist(repoId, namespace))
       .map(_ => repoId)
   }

  private val malformedRequestContentRejectionHandler = RejectionHandler.newBuilder().handle {
    case MalformedRequestContentRejection(msg, _) => complete((StatusCodes.BadRequest, msg))
  }.result()

  private def createRepo(namespace: Namespace, repoId: RepoId): Route =
    handleRejections(malformedRequestContentRejectionHandler) {
      entity(as[CreateRepositoryRequest]) { request =>
        createRepo(namespace, repoId, request.keyType)
      }
    } ~ createRepo(namespace, repoId, defaultKeyType)

  private def addTargetItem(namespace: Namespace, item: TargetItem): Future[SignedPayload[Json]] =
    for {
      result <- signedRoleGeneration.addTargetItem(item)
      _ <- tufTargetsPublisher.targetAdded(namespace, item)
    } yield result

  private def addTarget(namespace: Namespace, filename: TargetFilename, repoId: RepoId, clientItem: RequestTargetItem): Route =
    complete {
      val custom = for {
        name <- clientItem.name
        version <- clientItem.version
      } yield TargetCustom(name, version, clientItem.hardwareIds, clientItem.targetFormat, uri = Option(clientItem.uri.toURI))

      addTargetItem(namespace, TargetItem(repoId, filename, Option(clientItem.uri), clientItem.checksum, clientItem.length, custom))
    }

  private def addTargetFromContent(namespace: Namespace, filename: TargetFilename, repoId: RepoId): Route = {
    targetCustomParameters { custom =>
      withSizeLimit(userRepoSizeLimit) {
        fileUpload("file") { case (_, file) =>
          complete {
            for {
              item <- targetStore.store(repoId, filename, file, custom)
              result <- addTargetItem(namespace, item)
            } yield result
          }
        }
      } ~
      parameter('fileUri) { fileUri =>
        complete {
          for {
            item <- targetStore.storeFromUri(repoId, filename, Uri(fileUri), custom)
            result <- addTargetItem(namespace, item)
          } yield result
        }
      }
    }
  }

  private def findRootByVersion(repoId: RepoId, version: Int): Route = {
    respondWithHeader(RawHeader("x-ats-tuf-repo-id", repoId.uuid.toString)) { // TODO: Can be refactored ?
      complete(keyserverClient.fetchRootRole(repoId, version))
    }
  }

  private def findRole(repoId: RepoId, roleType: RoleType): Route = {
    onSuccess(signedRoleGeneration.findRole(repoId, roleType)) { signedRole =>
      respondWithCheckSum(signedRole.checksum.hash) {
        respondWithHeader(RawHeader("x-ats-tuf-repo-id", repoId.uuid.toString)) {
          complete(signedRole.content)
        }
      }
    }
  }


  private def modifyRepoRoutes(repoId: RepoId) =
    namespaceValidation(repoId) { namespace =>
      pathPrefix("root") {
        pathEnd {
          get {
            complete(keyserverClient.fetchUnsignedRoot(repoId))
          } ~
          (post & entity(as[SignedPayload[RootRole]])) { signedPayload =>
            complete(keyserverClient.updateRoot(repoId, signedPayload))
          }
        } ~
         path("private_keys" / KeyIdPath) { keyId =>
           delete {
             complete(keyserverClient.deletePrivateKey(repoId, keyId))
           } ~
           get {
             val f = keyserverClient.fetchKeyPair(repoId, keyId)
             complete(f)
           }
         }
      } ~
      (get & path(IntNumber ~ ".root.json")) { version ⇒
        findRootByVersion(repoId, version)
      } ~
      (get & path(JsonRoleTypeMetaPath)) { roleType =>
        findRole(repoId, roleType)
      } ~
       pathPrefix("targets") {
        path(TargetFilenamePath) { filename =>
          post {
            entity(as[RequestTargetItem]) { clientItem =>
              addTarget(namespace, filename, repoId, clientItem)
            }
          } ~
          put {
            addTargetFromContent(namespace, filename, repoId)
          } ~
          get {
            complete(targetStore.retrieve(repoId, filename))
          }
        } ~
        (pathEnd & put & entity(as[SignedPayload[TargetsRole]])) { signedPayload =>
          extractRoleChecksumHeader { checksum =>
            onSuccess(offlineSignedRoleStorage.saveTargetRole(namespace, signedPayload, repoId, checksum)) {
              case Valid((targetItems, newSignedRole)) =>
                tufTargetsPublisher.newTargetsAdded(namespace, signedPayload.signed.targets, targetItems)
                respondWithCheckSum(newSignedRole.checksum.hash) {
                  complete(StatusCodes.NoContent)
                }
              case Invalid(errors) =>
                complete(StatusCodes.BadRequest -> Json.obj("errors" -> errors.asJson))
            }
          }
        }
      }
    }

  val route =
    (pathPrefix("user_repo") & namespaceValidation.extractor) { namespace =>
      (post & pathEnd) {
        val repoId = RepoId.generate()
        createRepo(namespace, repoId)
      } ~
      UserRepoId(namespace) { repoId =>
        modifyRepoRoutes(repoId)
      }
    } ~
    (pathPrefix("repo" / RepoId.Path) & namespaceValidation.extractor) { (repoId, namespace) =>
      (pathEnd & post) {
        createRepo(namespace, repoId)
      } ~
      modifyRepoRoutes(repoId)
    }

}
