package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{StatusCodes, Uri}
import akka.http.scaladsl.server._
import akka.http.scaladsl.unmarshalling.PredefinedFromStringUnmarshallers.CsvSeq
import akka.http.scaladsl.unmarshalling._
import com.advancedtelematic.libats.data.RefinedUtils._
import com.advancedtelematic.libats.http.Errors.MissingEntity
import com.advancedtelematic.libats.http.RefinedMarshallingSupport._
import com.advancedtelematic.libats.http.UUIDKeyAkka._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TargetCustom, TargetsRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libats.http.RefinedMarshallingSupport._
import com.advancedtelematic.libats.http.AnyvalMarshallingSupport._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, RepoId, TargetFilename, _}
import com.advancedtelematic.libtuf_server.data.Marshalling._
import com.advancedtelematic.libtuf_server.data.Requests.{CommentRequest, CreateRepositoryRequest, _}
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient.RequestTargetItem
import com.advancedtelematic.tuf.reposerver.Settings
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType._
import com.advancedtelematic.tuf.reposerver.db._
import com.advancedtelematic.tuf.reposerver.delegations.DelegationsManagement
import com.advancedtelematic.tuf.reposerver.http.Errors.NoRepoForNamespace
import com.advancedtelematic.tuf.reposerver.http.RoleChecksumHeader._
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import org.slf4j.LoggerFactory
import slick.jdbc.MySQLProfile.api._

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class RepoResource(keyserverClient: KeyserverClient, namespaceValidation: NamespaceValidation,
                   targetStore: TargetStore, tufTargetsPublisher: TufTargetsPublisher)
                  (implicit val db: Database, val ec: ExecutionContext) extends Directives
  with TargetItemRepositorySupport
  with RepoNamespaceRepositorySupport
  with FilenameCommentRepository.Support
  with SignedRoleRepositorySupport
  with Settings {

  private implicit val signedRoleGeneration = SignedRoleGeneration(keyserverClient)
  private val offlineSignedRoleStorage = new OfflineSignedRoleStorage(keyserverClient)
  private val targetRoleGeneration = new TargetRoleEdit(keyserverClient, signedRoleGeneration)
  private val delegations = new DelegationsManagement()

  val log = LoggerFactory.getLogger(this.getClass)

  private val targetCustomParameters: Directive1[TargetCustom] =
    parameters(
      'name.as[TargetName],
      'version.as[TargetVersion],
      'hardwareIds.as(CsvSeq[HardwareIdentifier]).?(immutable.Seq.empty[HardwareIdentifier]),
      'targetFormat.as[TargetFormat].?
    ).tmap { case (name, version, hardwareIds, targetFormat) =>
      TargetCustom(name, version, hardwareIds, targetFormat.orElse(Some(TargetFormat.BINARY)))
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
    } ~ createRepo(namespace, repoId, KeyType.default)

  private def addTargetItem(namespace: Namespace, item: TargetItem): Future[JsonSignedPayload] =
    for {
      result <- targetRoleGeneration.addTargetItem(item)
      _ <- tufTargetsPublisher.targetAdded(namespace, item)
    } yield result

  private def addTarget(namespace: Namespace, filename: TargetFilename, repoId: RepoId, clientItem: RequestTargetItem): Route =
    complete {
      val targetFormat = clientItem.targetFormat.orElse(Some(TargetFormat.BINARY))
      val custom = for {
        name <- clientItem.name
        version <- clientItem.version
      } yield TargetCustom(name, version, clientItem.hardwareIds, targetFormat, uri = Option(clientItem.uri.toURI))

      addTargetItem(namespace, TargetItem(repoId, filename, Option(clientItem.uri), clientItem.checksum, clientItem.length, custom, StorageMethod.Unmanaged))
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

  private def withRepoIdHeader(repoId: RepoId) = respondWithHeader(RawHeader("x-ats-tuf-repo-id", repoId.uuid.toString))

  private def findRootByVersion(repoId: RepoId, version: Int): Route = {
    complete(keyserverClient.fetchRootRole(repoId, version))
  }

  private def findRole(repoId: RepoId, roleType: RoleType): Route = {
    onSuccess(signedRoleGeneration.findRole(repoId, roleType)) { signedRole =>
      respondWithCheckSum(signedRole.checksum.hash) {
        complete(signedRole.content)
      }
    }
  }

  def findComment(repoId: RepoId, filename: TargetFilename): Route =
    complete {
      filenameCommentRepo.find(repoId, filename).map(CommentRequest)
    }

  def findComments(repoId: RepoId): Route =
    complete {
      filenameCommentRepo.find(repoId).map {
        _.map {
          case (filename, comment) => FilenameComment(filename, comment)
        }
      }
    }

  def addComment(repoId: RepoId, filename: TargetFilename, commentRequest: CommentRequest): Route =
    complete {
      targetItemRepo.findByFilename(repoId, filename).flatMap { _ =>
        filenameCommentRepo.persist(repoId, filename, commentRequest.comment)
      }
    }

  def deleteTargetItem(repoId: RepoId, filename: TargetFilename): Route = complete {
    for {
      _ <- targetStore.delete(repoId, filename)
      _ <- targetRoleGeneration.deleteTargetItem(repoId, filename)
    } yield StatusCodes.NoContent
  }

  def saveOfflineTargetsRole(repoId: RepoId, namespace: Namespace, signedPayload: SignedPayload[TargetsRole],
                             checksum: Option[RoleChecksum]): Future[SignedRole[TargetsRole]] = for {
    (newItems, newSignedRole) <- offlineSignedRoleStorage.saveTargetRole(targetStore)(repoId, signedPayload, checksum)
    _ <- tufTargetsPublisher.newTargetsAdded(namespace, signedPayload.signed.targets, newItems)
  } yield newSignedRole

  private def modifyRepoRoutes(repoId: RepoId): Route =
    (namespaceValidation(repoId) & withRepoIdHeader(repoId)){ namespace =>
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
      path("delegations" / DelegatedRoleUriPath) { delegatedRoleName =>
        (put & entity(as[SignedPayload[TargetsRole]])) { payload =>
          complete(delegations.create(repoId, delegatedRoleName, payload).map(_ => StatusCodes.NoContent))
        } ~
        get {
          complete(delegations.find(repoId, delegatedRoleName))
        }
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
          } ~
          delete {
            deleteTargetItem(repoId, filename)
          }
        } ~
        (pathEnd & put & entity(as[SignedPayload[TargetsRole]])) { signedPayload =>
          extractRoleChecksumHeader { checksum =>
            onSuccess(saveOfflineTargetsRole(repoId, namespace, signedPayload, checksum)) { newSignedRole =>
              respondWithCheckSum(newSignedRole.checksum.hash) {
                complete(StatusCodes.NoContent)
              }
            }
          }
        }
      } ~
      pathPrefix("comments") {
        pathEnd {
          findComments(repoId)
        } ~
        pathPrefix(TargetFilenamePath) { filename =>
          put {
            entity(as[CommentRequest]) { commentRequest =>
              addComment(repoId, filename, commentRequest)
            }
          } ~
          get {
            findComment(repoId, filename)
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
