package com.advancedtelematic.tuf.reposerver.http

import akka.actor.Scheduler
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.Multipart.BodyPart
import akka.http.scaladsl.model.headers.{RawHeader, `Content-Length`}
import akka.http.scaladsl.model.{EntityStreamException, HttpEntity, Multipart, ParsingException, StatusCodes, Uri}
import akka.http.scaladsl.server._
import akka.http.scaladsl.unmarshalling.PredefinedFromStringUnmarshallers.CsvSeq
import akka.http.scaladsl.unmarshalling._
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import com.advancedtelematic.libats.codecs.CirceCodecs.checkSumCodec
import com.advancedtelematic.libats.data.RefinedUtils._
import com.advancedtelematic.libats.http.Errors.{EntityAlreadyExists, MissingEntity}
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
import com.advancedtelematic.libats.data.{ErrorRepresentation, Limit, Offset}
import com.advancedtelematic.libats.http.FromLongUnmarshallers._
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, RepoId, TargetFilename, _}
import com.advancedtelematic.libtuf.data.ValidationUtils
import com.advancedtelematic.libtuf_server.data.Marshalling._
import com.advancedtelematic.libtuf_server.data.Requests.{CommentRequest, CreateRepositoryRequest, _}
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient.RootRoleNotFound
import com.advancedtelematic.libtuf_server.repo.client.ReposerverClient.RequestTargetItem
import com.advancedtelematic.libtuf_server.repo.server.DataType.SignedRole
import com.advancedtelematic.tuf.reposerver.Settings
import com.advancedtelematic.libtuf_server.repo.server.DataType._
import com.advancedtelematic.libtuf_server.repo.server.RepoRoleRefresh
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType._
import com.advancedtelematic.tuf.reposerver.db._
import com.advancedtelematic.tuf.reposerver.delegations.DelegationsManagement
import com.advancedtelematic.tuf.reposerver.http.Errors.{NoRepoForNamespace, RequestCanceledByUpstream}
import com.advancedtelematic.tuf.reposerver.http.RoleChecksumHeader._
import com.advancedtelematic.tuf.reposerver.target_store.TargetStore
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import org.slf4j.LoggerFactory
import slick.jdbc.MySQLProfile.api._

import scala.async.Async._
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class RepoResource(keyserverClient: KeyserverClient, namespaceValidation: NamespaceValidation,
                   targetStore: TargetStore, tufTargetsPublisher: TufTargetsPublisher)
                  (implicit val db: Database, val ec: ExecutionContext, val scheduler: Scheduler) extends Directives
  with TargetItemRepositorySupport
  with RepoNamespaceRepositorySupport
  with FilenameCommentRepository.Support
  with SignedRoleRepositorySupport
  with Settings {

  private implicit val signedRoleGeneration = TufRepoSignedRoleGeneration(keyserverClient)
  private val offlineSignedRoleStorage = new OfflineSignedRoleStorage(keyserverClient)
  private val roleRefresher = new RepoRoleRefresh(keyserverClient, new TufRepoSignedRoleProvider(), new TufRepoTargetItemsProvider())
  private val targetRoleGeneration = new TargetRoleEdit(signedRoleGeneration)
  private val delegations = new DelegationsManagement()
  implicit val limitUnmarshaller: Unmarshaller[String, Limit] = getLimitUnmarshaller()

  /*
    extractRequestEntity is needed for tests only. We should get this value from the `Content-Length` header.
    Http clients send this as a header, but akka-http-testkit takes the header and makes it available only
    through this method.
  */
  private val withContentLengthCheck: Directive1[Long] =
    (optionalHeaderValueByType[`Content-Length`](()) & extractRequestEntity)
      .tmap { case (clHeader, entity) => clHeader.map(_.length).orElse(entity.contentLengthOption) }
      .flatMap {
        case Some(cl) if cl <= outOfBandUploadLimit => provide(cl)
        case Some(cl) => failWith(Errors.PayloadTooLarge(cl, outOfBandUploadLimit))
        case None => reject(MissingHeaderRejection("Content-Length"))
      }

  private val withMultipartUploadFileSizeCheck: Directive1[Long] =
    parameter('fileSize.as[Long]).flatMap { fileSize =>
      if (fileSize > outOfBandUploadLimit)
        failWith(Errors.PayloadTooLarge(fileSize, outOfBandUploadLimit))
      else provide(fileSize)
    }

  private val withMultipartUploadPartSizeCheck: Directive1[Long] =
    parameters('contentLength.as[Int], 'part.as[Int])
      .tmap { case (partContentLength, partNumber) =>
        val totalFileSize = multipartUploadPartSize * (partNumber - 1) + partContentLength
        (partContentLength, totalFileSize)
      }
      .tflatMap {
        case (partContentLength, _) if partContentLength > multipartUploadPartSize =>
          failWith(Errors.FilePartTooLarge(partContentLength, multipartUploadPartSize))
        case (_, totalFileSize) if totalFileSize > outOfBandUploadLimit =>
          failWith(Errors.PayloadTooLarge(totalFileSize, outOfBandUploadLimit))
        case (_, totalFileSize) =>
          provide(totalFileSize)
      }

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
      _ <- validateTargetItem(item)
      result <- targetRoleGeneration.addTargetItem(item)
      _ <- tufTargetsPublisher.targetAdded(namespace, item)
    } yield result

  private def validateTargetItem(item: TargetItem): Future[Unit] = {
    val notAllowedCharsInName = ValidationUtils.findNotAllowedCharacters(item.filename.value)
    val notAllowedCharsInCustom = item.custom.toList.flatMap { item =>
      ValidationUtils.findNotAllowedCharacters(item.uri.map(_.toString).getOrElse("")) ++
        ValidationUtils.findNotAllowedCharacters(item.name.value) ++
        ValidationUtils.findNotAllowedCharacters(item.version.value) ++
        ValidationUtils.findNotAllowedCharacters(item.proprietary.noSpaces)
    }

    val notAllowedChars = notAllowedCharsInName ++ notAllowedCharsInCustom
    if (notAllowedChars.nonEmpty) Future.failed(Errors.NotSupportedChars(notAllowedChars.distinct))
    else Future.successful(())
  }

  private def addTarget(namespace: Namespace, filename: TargetFilename, repoId: RepoId, clientItem: RequestTargetItem): Route =
    complete {
      val targetFormat = clientItem.targetFormat.orElse(Some(TargetFormat.BINARY))
      val custom = for {
        name <- clientItem.name
        version <- clientItem.version
      } yield TargetCustom(name, version, clientItem.hardwareIds, targetFormat, uri = Option(clientItem.uri.toURI))

      addTargetItem(namespace, TargetItem(repoId, filename, Option(clientItem.uri), clientItem.checksum, clientItem.length, custom, StorageMethod.Unmanaged))
    }

  private def storeTarget(ns: Namespace, repoId: RepoId, filename: TargetFilename, custom: TargetCustom,
                          file: Source[ByteString, Any], size: Option[Long]): Future[JsonSignedPayload] = {
    for {
      item <- targetStore.store(repoId, filename, file, custom, size)
      result <- addTargetItem(ns, item)
    } yield result
  }

  private def addTargetFromContent(namespace: Namespace, filename: TargetFilename, repoId: RepoId): Route = {
    targetCustomParameters { custom =>
      concat(
        withSizeLimit(userRepoSizeLimit) {
          withRequestTimeout(userRepoUploadRequestTimeout) {
            extractRequestContext { ctx =>
              import ctx.materializer
              entity(as[Multipart.FormData]) { formData =>
                val resultF = formData.parts.mapAsync[Option[TargetItem]](1) {
                  case b: BodyPart if b.name == "file" =>
                    targetStore.store(repoId, filename, b.entity.dataBytes, custom, size = None).map(item => Some(item))
                  case b: BodyPart =>
                    b.entity.discardBytes().future().map(_ => None)
                }
                  .collect { case Some(targetItem: TargetItem) => targetItem }
                  .runWith(Sink.headOption).flatMap {
                    case Some(item) => addTargetItem(namespace, item)
                    case None => Future.failed(Errors.FailedSoftwareUpload)
                }
                onSuccess(resultF) { result =>
                  log.info(s"Successfully uploaded $filename for repoId $repoId")
                  complete(result)
                }
              }
            }
          }
        },
        (withSizeLimit(userRepoSizeLimit) & withRequestTimeout(userRepoUploadRequestTimeout) & extractRequestEntity) { entity =>
          entity.contentLengthOption match {
            case Some(size) if size > 0 => complete(storeTarget(namespace, repoId, filename, custom, entity.dataBytes, Option(size)).map(_ => StatusCodes.NoContent))
            case _ => reject(MissingHeaderRejection("Content-Length"))
          }
        },
        parameter('fileUri) { fileUri =>
          complete {
            for {
              item <- targetStore.storeFromUri(repoId, filename, Uri(fileUri), custom)
              result <- addTargetItem(namespace, item)
            } yield result
          }
        })
    }
  }

  private def withRepoIdHeader(repoId: RepoId) = respondWithHeader(RawHeader("x-ats-tuf-repo-id", repoId.uuid.toString))

  private def findRootByVersion(repoId: RepoId, version: Int): Route = {
    complete(
      keyserverClient.fetchRootRole(repoId, version)
        .map[ToResponseMarshallable](root => StatusCodes.OK -> root)
        .recover[ToResponseMarshallable] {
          case e@RootRoleNotFound => e.responseCode -> ErrorRepresentation(e.code, e.desc, None, Some(e.errorId))
        }
    )
  }

  private def findRole(repoId: RepoId, roleType: RoleType): Route = {
    onSuccess(signedRoleGeneration.findRole(repoId, roleType, roleRefresher)) { signedRole =>
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

  def deleteOsTreeTargets(repoId: RepoId, namespace: Namespace): Route = complete {
    for {
      _ <- targetRoleGeneration.deleteOsTreeTargets(repoId)
      _ <- tufTargetsPublisher.deleteOsTreeTargets(namespace)
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
      pathPrefix("uploads") {
        (put & path(TargetFilenamePath) & withContentLengthCheck & parameter('force.as[Boolean] ? false)) { (filename, cl, force) =>
          val f = async {
            if(await(targetItemRepo.exists(repoId, filename)) && !force)
              throw new EntityAlreadyExists[TargetItem]()
            await(targetStore.buildStorageUrl(repoId, filename, cl))
          }
          onSuccess(f) { uri =>
            redirect(uri, StatusCodes.Found)
          }
        } ~
        (get & path(TargetFilenamePath)) { fileName =>
            complete(targetStore.retrieveFromManaged(repoId, fileName))
        }
      } ~
      pathPrefix("multipart") {
        (post & path("initiate" / TargetFilenamePath) & withMultipartUploadFileSizeCheck & parameter('force.as[Boolean] ? false)) { (fileName, _, force) =>
          val rs = for {
            exists <- if (force) Future.successful(false) else targetItemRepo.exists(repoId, fileName)
            result <- if (exists) Future.failed(new EntityAlreadyExists[TargetItem]()) else targetStore.initiateMultipartUpload(repoId, fileName)
          } yield result
          complete(rs)
        } ~
        (get & path("url" / TargetFilenamePath) & parameters('part, 'uploadId.as[MultipartUploadId], 'md5, 'contentLength.as[Int])) {
          (filename, partId, uploadId, md5, contentLength) =>
            withMultipartUploadPartSizeCheck { _ =>
              complete(targetStore.buildSignedURL(repoId, filename, uploadId, partId, md5, contentLength))
            }
        } ~
        (put & path("complete" / TargetFilenamePath)) { filename =>
          entity(as[CompleteUploadRequest]) { body =>
            complete(targetStore.completeMultipartUpload(repoId, filename, body.uploadId, body.partETags))
          }
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
            handleExceptions {
              ExceptionHandler {
                case e: EntityStreamException if e.getMessage.contains("The HTTP parser was receiving an entity when the underlying connection was closed unexpectedly") =>
                  failWith(RequestCanceledByUpstream(e))
                case e: ParsingException if e.getMessage.contains("Unexpected end of multipart entity") =>
                  failWith(RequestCanceledByUpstream(e))
              }
            }(addTargetFromContent(namespace, filename, repoId))
          } ~
          head {
            onComplete(targetStore.find(repoId, filename)) {
              case Success(_) => complete((StatusCodes.OK, HttpEntity.Empty))
              case Failure(e@Errors.TargetNotFoundError) => complete((e.responseCode, HttpEntity.Empty))
              case Failure(e) => failWith(e)
            }
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
                respondWithHeader(RawHeader("x-ats-targets-role-size-limit", targetsFileSizeLimit.toString)) {
                  complete(StatusCodes.NoContent)
                }
              }
            }
          }
        }
      } ~
      pathPrefix("ostree") {
        (pathEnd & delete) {
            deleteOsTreeTargets(repoId, namespace)
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
    } ~
    (path("repos") & parameters('offset.as[Offset], 'limit.as[Limit]) ) { (offset, limit) =>
      complete(repoNamespaceRepo.list(offset, limit))
    }
}
