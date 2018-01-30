package com.advancedtelematic.tuf.reposerver.http

import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType._
import akka.http.scaladsl.unmarshalling._
import PredefinedFromStringUnmarshallers.CsvSeq
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.headers.{RawHeader}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes, Uri}
import akka.http.scaladsl.server._
import cats.data.Validated.{Invalid, Valid}
import com.advancedtelematic.libats.data.RefinedUtils._
import com.advancedtelematic.libats.http.Errors.MissingEntity
import com.advancedtelematic.libats.messaging.MessageBusPublisher
import com.advancedtelematic.libtuf_server.data.Messages.TufTargetAdded
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, RepoId}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, TargetItem}
import com.advancedtelematic.tuf.reposerver.db.{RepoNamespaceRepositorySupport, SignedRoleRepositorySupport, TargetItemRepositorySupport}
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepository.SignedRoleNotFound
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TargetCustom, TargetsRole}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore
import com.advancedtelematic.libats.http.RefinedMarshallingSupport._
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.libats.http.AnyvalMarshallingSupport._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.codecs.CirceCodecs._
import com.advancedtelematic.libats.data.DataType.{Checksum, Namespace, ValidChecksum}
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient.RequestTargetItem
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient.RequestTargetItem._
import com.advancedtelematic.libats.http.UUIDKeyPath._
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.Settings
import com.advancedtelematic.libtuf_server.data.Marshalling._
import com.advancedtelematic.tuf.reposerver.http.Errors.NoRepoForNamespace
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import eu.timepit.refined.api.Refined
import io.circe.Json
import io.circe.syntax._
import org.slf4j.LoggerFactory

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import slick.jdbc.MySQLProfile.api._
import RoleChecksumHeader._

class RepoResource(keyserverClient: KeyserverClient, namespaceValidation: NamespaceValidation,
                   targetStore: TargetStore, messageBusPublisher: MessageBusPublisher)
                  (implicit val db: Database, val ec: ExecutionContext) extends Directives
  with TargetItemRepositorySupport
  with SignedRoleRepositorySupport
  with RepoNamespaceRepositorySupport
  with Settings {

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

  private def createRepo(namespace: Namespace, repoId: RepoId): Route =
   complete {
     repoNamespaceRepo.ensureNotExists(namespace)
       .flatMap(_ => keyserverClient.createRoot(repoId))
       .flatMap(_ => repoNamespaceRepo.persist(repoId, namespace))
       .map(_ => repoId)
   }

  private def addTargetItem(namespace: Namespace, item: TargetItem): Future[SignedPayload[Json]] =
    for {
      result <- signedRoleGeneration.addTargetItem(item)
      _ <- messageBusPublisher.publish(TufTargetAdded(namespace, item.filename, item.checksum, item.length, item.custom))
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

  private def storeSignedTarget[T](repoId: RepoId, signed: SignedPayload[TargetsRole]): Route = {
    val f: Future[ToResponseMarshallable] = offlineSignedRoleStorage.store(repoId, signed).map {
      case Valid(_) =>
        StatusCodes.NoContent
      case Invalid(errors) =>
        val obj = Json.obj("errors" -> errors.asJson)
        StatusCodes.BadRequest -> obj
    }

    complete(f)
  }

  private def updateSignedTarget(repoId: RepoId, signedPayload: SignedPayload[TargetsRole], existentRole: SignedRole) =
    extractRoleChecksumHeader {
      case Some(checksum) if existentRole.checksum.hash == checksum =>
        onSuccess(offlineSignedRoleStorage.store(repoId, signedPayload)) {
          case Valid(newSignedRole) =>
            respondWithCheckSum(newSignedRole.checksum.hash) {
              complete(StatusCodes.NoContent)
            }
          case Invalid(errors) =>
            val obj = Json.obj("errors" -> errors.asJson)
            complete(StatusCodes.BadRequest -> obj)
        }
      case Some(_) =>
        failWith(Errors.RoleChecksumMismatch) // TODO: Should be 412?
      case None =>
        failWith(Errors.RoleChecksumNotProvided)
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
      (put & path("keys" / "targets") & entity(as[TufKey])) { key =>
        val f = keyserverClient.addTargetKey(repoId, key).map(_ => StatusCodes.NoContent)
        complete(f)
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
        (pathEnd & put & entity(as[SignedPayload[TargetsRole]])) { signed =>
          onComplete(signedRoleRepo.find(repoId, RoleType.TARGETS)) {
            case Success(signedRole) =>
              updateSignedTarget(repoId, signed, signedRole)
            case Failure(SignedRoleNotFound) =>
              storeSignedTarget(repoId, signed)
            case Failure(t) =>
              failWith(t)
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
    pathPrefix("repo" / RepoId.Path) { repoId =>
      (pathEnd & post & namespaceValidation.extractor) { namespace =>
        createRepo(namespace, repoId)
      } ~
        modifyRepoRoutes(repoId)
    }
}
