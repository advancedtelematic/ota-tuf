package com.advancedtelematic.tuf.cli.repo

import java.io._
import java.net.URI
import java.nio.file.attribute.PosixFilePermission._
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.time.{Instant, Period}

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import com.advancedtelematic.libats.data.DataType.{HashMethod, ValidChecksum}
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.crypt.KeyListToKeyMapOps._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.TufRole._
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, DelegatedPathPattern, DelegatedRoleName, Delegations, MetaPath, RootRole, TargetCustom, TargetsRole, TufRole, TufRoleOps}
import com.advancedtelematic.libtuf.data.RootManipulationOps._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.{ClientSignature, HardwareIdentifier, KeyId, KeyType, RoleType, SignedPayload, TargetFilename, TargetName, TargetVersion, TufKey, TufKeyPair, TufPrivateKey, ValidTargetFilename}
import com.advancedtelematic.libtuf.data.{ClientDataType, RootRoleValidation}
import com.advancedtelematic.libtuf.http.TufServerHttpClient.{RoleNotFound, TargetsResponse}
import com.advancedtelematic.libtuf.http._
import com.advancedtelematic.tuf.cli.DataType.{KeyName, OAuthConfig, RepoConfig, _}
import com.advancedtelematic.tuf.cli.Errors.{CommandNotSupportedByRepositoryType, PastDate}
import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.tuf.cli.repo.TufRepo.TargetsPullError
import com.advancedtelematic.tuf.cli.{CliCodecs, CliUtil}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.refineV
import io.circe.jawn.parseFile
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import org.slf4j.LoggerFactory
import shapeless.{:: => _, Path => _}

import scala.async.Async._
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

object TufRepo {
  import CliCodecs._

  def apply(repoServerType: TufServerType, repoPath: Path)(implicit ec: ExecutionContext) =
    repoServerType match {
      case RepoServer => new RepoServerRepo(repoPath)
      case Director => new DirectorRepo(repoPath)
    }

  case object RoleChecksumNotFound extends Exception(
    "Could not find targets checksum file. You need this to push a new targets file. A Role checksum can be obtained using the pull command"
  ) with NoStackTrace

  case class TreehubConfigError(msg: String) extends Exception(msg) with NoStackTrace

  case class MissingCredentialsZipFile(filename: String) extends Exception(s"Missing file from credentials.zip: $filename") with NoStackTrace

  case class RepoAlreadyInitialized(path: Path) extends Exception(s"Repository at $path was already initialized") with NoStackTrace

  case class TargetsPullError(msg: String) extends Exception(s"Could not pull targets from server $msg") with NoStackTrace

  case class RoleMissing[T](rolePath: String)(implicit ev: TufRole[T]) extends Exception(s"Missing role ${ev.metaPath.toString()} at $rolePath") with NoStackTrace

  case class RootPullError(errors: NonEmptyList[String]) extends Exception("Could not validate a valid root.json chain:\n" + errors.toList.mkString("\n")) with NoStackTrace

  protected [cli] def readConfig(repoPath: => Path): Try[RepoConfig] =
    Try { new FileInputStream(repoPath.resolve("config.json").toFile) }
      .flatMap { is => CliUtil.readJsonFrom[RepoConfig](is) }

  def importTlsCerts(repoPath: Path, clientCert: Path, serverCert: Option[Path]): Try[RepoConfig] = {
    for {
      oldConfig <- readConfig(repoPath)
      newConfig <- Try {
        val clientCertPath = repoPath.resolve("client_auth.p12")
        Files.copy(clientCert, clientCertPath)

        val serverCertPath = serverCert.map { serverCertPath =>
          val p = repoPath.resolve("server.p12")
          Files.copy(serverCertPath, p)
          p
        }

        oldConfig.copy(auth = Some(MutualTlsConfig(clientCertPath.getFileName, serverCertPath.map(_.getFileName))))
      }
      _ <- writeConfig(repoPath, newConfig)
    } yield newConfig
  }

  protected [cli] def writeConfig(repoPath: Path, config: RepoConfig): Try[Unit] = Try {
    Files.write(repoPath.resolve("config.json"), config.asJson.spaces2.getBytes)
  }
}

abstract class TufRepo[S <: TufServerClient](val repoPath: Path)(implicit ec: ExecutionContext) {
  import TufRepo._

  protected[repo] lazy val keyStorage = CliKeyStorage.forRepo(repoPath)

  protected lazy val log = LoggerFactory.getLogger(this.getClass)

  private lazy val rolesPath = repoPath.resolve("roles")

  def initRepoDirs(): Try[Unit] = Try {
    val perms = PosixFilePermissions.asFileAttribute(java.util.EnumSet.of(OWNER_READ, OWNER_WRITE, OWNER_EXECUTE))

    Files.createDirectory(repoPath, perms)
    Files.createDirectory(rolesPath, perms)
    Files.createDirectory(rolesPath.resolve("unsigned"), perms)

    def checkPerms(path: Path): Unit = {
      val currentPerms = Files.getPosixFilePermissions(path)
      if(currentPerms.asScala != Set(OWNER_READ, OWNER_WRITE, OWNER_EXECUTE))
        log.warn(s"Permissions for $path are too open")
    }

    checkPerms(repoPath)
    checkPerms(rolesPath)
    checkPerms(rolesPath.resolve("unsigned"))
  }

  def keyIdsByName(keyNames: List[KeyName]): Try[List[KeyId]] = for {
    keys <- keyNames.traverse(keyStorage.readPublicKey)
  } yield keys.map(_.id)

  def removeRootKeys(keyIds: List[KeyId]): Try[Path] = for {
    unsignedRoot <- readUnsignedRole[RootRole]
    newRoot = unsignedRoot.removeRoleKeys(RoleType.ROOT, keyIds.toSet)
    path <- writeUnsignedRole(newRoot)
  } yield path

  def addRootKeys(keyNames: List[KeyName]): Try[Path] =
    for {
      unsignedRoot <- readUnsignedRole[RootRole]
      keys <- keyNames.traverse(keyStorage.readPublicKey)
      newRoot = unsignedRoot.addRoleKeys(RoleType.ROOT, keys:_*)
      path <- writeUnsignedRole(newRoot)
    } yield path

  private def validateSameVersion(from: SignedPayload[RootRole], to: SignedPayload[RootRole]): Future[Unit] = {
    if(from.asJson.canonical == to.asJson.canonical)
      Future.successful(())
    else
      Future.failed(RootPullError(NonEmptyList.of("New root has same version as old root but is not the same root.json")))
  }

  private def validatePath(reposerverClient: S, from: SignedPayload[RootRole], to: SignedPayload[RootRole]): Future[Unit] = {
    val rootRolesF = fetchRootChain(reposerverClient, from.signed.version + 1, to.signed.version)

    rootRolesF.flatMap { rootRoles =>
      val validated = (from +: rootRoles :+ to).sliding(2, 1).toList.traverse {
        case Vector(oldR, newR) =>
          RootRoleValidation.newRootIsValid(newR, oldR)
        case l =>
          throw new IllegalArgumentException(s"too many elements for $l")
      }

      validated.fold(
        errors => Future.failed(RootPullError(errors)),
        _ => Future.successful(())
      )
    }
  }

  def pullRoot(client: S, skipLocalValidation: Boolean): Future[SignedPayload[RootRole]] = {
    def newRootIsvalid(newRoot: SignedPayload[RootRole]): Future[SignedPayload[RootRole]] = {
      if(skipLocalValidation)
        RootRoleValidation.rootIsValid(newRoot) match {
          case Invalid(errors) => Future.failed(RootPullError(errors))
          case Valid(signedPayload) => Future.successful(signedPayload)
        }
      else
        async {
          val oldRoot = await(readSignedRole[RootRole].toFuture)

          if (oldRoot.signed.version == newRoot.signed.version)
            await(validateSameVersion(oldRoot, newRoot))
          else
            await(validatePath(client, oldRoot, newRoot))

          newRoot
        }
    }

    for {
      newRoot <- client.root()
      _ <- newRootIsvalid(newRoot)
      _ <- writeUnsignedRole(newRoot.signed).toFuture
      _ <- writeSignedRole(newRoot).toFuture
    } yield newRoot
  }

  def pushRoot(client: S): Future[Unit] = for {
    signedRoot <- readSignedRole[RootRole].toFuture
    _ <- client.pushSignedRoot(signedRoot)
  } yield ()

  def signRoot(keys: Seq[KeyName], validExpiration: Instant => Instant, version: Option[Int] = None): Try[Path] =
    signRole[RootRole](version, keys, validExpiration)

  import cats.implicits._

  protected def signRole[T : Decoder : Encoder](version: Option[Int], keys: Seq[KeyName], validExpiration: Instant => Instant)
                                               (implicit tufRole: TufRole[T]): Try[Path] = {
    def signatures(payload: T): Try[List[ClientSignature]] =
      keys.toList.traverse { key =>
        keyStorage.readKeyPair(key).map { case (pub, priv) =>
          TufCrypto.signPayload(priv, payload.asJson).toClient(pub.id)
        }
      }

    for {
      unsigned <- readUnsignedRole[T]
      expiration <- Try(validExpiration(unsigned.expires))
      newUnsigned = tufRole.refreshRole(unsigned, oldV => version.getOrElse(oldV + 1), expiration)
      sigs <- signatures(newUnsigned)
      signedRole = SignedPayload(sigs, newUnsigned, newUnsigned.asJson)
      path <- writeSignedRole(signedRole)
    } yield path
  }

  protected def deleteOrReadKey(reposerverClient: S, keyName: KeyName, keyId: KeyId): Future[TufPrivateKey] = {
    keyStorage.readPrivateKey(keyName).toFuture.recoverWith { case _ =>
      log.info(s"Could not read old private key locally, fetching before deleting from server")

      for {
        keyPair ← reposerverClient.fetchKeyPair(keyId)
        _ ← reposerverClient.deleteKey(keyId)
      } yield keyPair.privkey
    }
  }

  private def withExistingRolePath[T : TufRole, U](path: Path)(fn: Path => Try[U]): Try[U] = {
    if (path.toFile.exists())
      fn(path)
    else
      Failure(RoleMissing[T](path.toString))
  }

  def readUnsignedRole[T : Decoder](implicit ev: TufRole[T]): Try[T] = {
    val path = rolesPath.resolve("unsigned").resolve(ev.metaPath.value)
    withExistingRolePath[T, T](path) { p =>
      parseFile(p.toFile).flatMap(_.as[T]).toTry
    }
  }

  def readSignedRole[T : Encoder : Decoder](implicit ev: TufRole[T]): Try[SignedPayload[T]] = {
    val path = rolesPath.resolve(ev.metaPath.value)
    withExistingRolePath[T, SignedPayload[T]](path) { p =>
      parseFile(p.toFile).flatMap(_.as[SignedPayload[T]]).toTry
    }
  }

  protected def readChecksum[T](implicit ev: TufRole[T]): Try[Refined[String, ValidChecksum]] = Try {
    val lines = Files.readAllLines(rolesPath.resolve(ev.checksumPath)).asScala
    assert(lines.tail.isEmpty)
    refineV[ValidChecksum](lines.head).valueOr(err => throw new Exception(err))
  }.recoverWith {
    case _: FileNotFoundException => Failure(RoleChecksumNotFound)
  }

  protected def writeChecksum(checksum: Refined[String, ValidChecksum]): Try[Unit] = Try {
    Files.write(rolesPath.resolve(TufRole.targetsTufRole.checksumPath), checksum.value.getBytes)
  }

  def writeUnsignedRole[T : TufRole : Encoder](role: T): Try[Path] =
    writeRole(rolesPath.resolve("unsigned"), role.metaPath, role)

  def writeSignedRole[T : TufRole : Encoder](signedPayload: SignedPayload[T]): Try[Path] =
    writeRole(rolesPath, signedPayload.signed.metaPath, signedPayload)

  private def writeRole[T : Encoder](path: Path, metaPath: MetaPath, payload: T): Try[Path] = Try {
    val rolePath = path.resolve(metaPath.value)
    Files.write(rolePath, payload.asJson.spaces2.getBytes)
    rolePath
  }

  def genKeys(name: KeyName, keyType: KeyType, keySize: Option[Int] = None): Try[TufKeyPair] =
    keyStorage.genKeys(name, keyType, keySize)

  def treehubConfig: Try[TreehubConfig] = TufRepo.readConfig(repoPath).map(_.treehub)

  def authConfig: Try[Option[CliAuth]] = TufRepo.readConfig(repoPath).map(_.auth)

  def repoServerUri: Try[URI] = TufRepo.readConfig(repoPath).map(_.reposerver)

  private def fetchRootChain(reposerverClient: S, from: Int, to: Int): Future[Vector[SignedPayload[RootRole]]] = {
    val versionsToFetch = from until to

    val rootRolesF = versionsToFetch.foldLeft(Future.successful(Vector.empty[SignedPayload[RootRole]])) { (accF, vv) =>
      for {
        acc <- accF
        payload <- reposerverClient.root(vv.some).recoverWith {
          case RoleNotFound(msg) => Future.failed(RootPullError(NonEmptyList.of(msg)))
        }
      } yield acc :+ payload
    }

    rootRolesF
  }

  def moveRootOffline(repoClient: S,
                      newRootName: KeyName,
                      oldRootName: KeyName,
                      oldKeyId: Option[KeyId],
                      newTargetsName: Option[KeyName],
                      rootExpireTime: Instant): Future[SignedPayload[RootRole]]

  def initTargets(version: Int, expires: Instant): Try[Path]

  def addTarget(targetFilename: TargetFilename, targetItem: ClientTargetItem): Try[Path]

  def deleteTarget(targetFilename: TargetFilename): Try[Path]

  def addTargetDelegation(name: DelegatedRoleName, key: List[TufKey],
                          delegatedPaths: List[DelegatedPathPattern], threshold: Int): Try[Path]

  def signTargets(targetsKeys: Seq[KeyName], expiration: Instant => Instant, version: Option[Int] = None): Try[Path]

  def pullVerifyTargets(reposerverClient: S, rootRole: RootRole): Future[SignedPayload[TargetsRole]]

  def pullTargets(reposerverClient: S): Future[SignedPayload[TargetsRole]]

  def pushTargets(reposerverClient: S): Future[SignedPayload[TargetsRole]]
}

class RepoServerRepo(repoPath: Path)(implicit ec: ExecutionContext) extends TufRepo[ReposerverClient](repoPath) {

  def initTargets(version: Int, expires: Instant): Try[Path] = {
    val emptyTargets = TargetsRole(expires, Map.empty, version)
    writeUnsignedRole(emptyTargets)
  }

  def ensureTargetsPulled(reposerverClient: ReposerverClient, rootRole: RootRole): Future[Unit] = {
    Future.fromTry {
      readUnsignedRole[TargetsRole].map(_ => ())
    }.recoverWith {
      case _ =>
        log.warn("unsigned targets not available locally, pulling targets from server")
        pullVerifyTargets(reposerverClient, rootRole).map(_ => ())
    }
  }

  private def writeTargets(targets: SignedPayload[TargetsRole], checksum: Refined[String, ValidChecksum]): Try[Unit] =
    writeUnsignedRole(targets.signed).flatMap(_ => writeChecksum(checksum))

  override def pullVerifyTargets(reposerverClient: ReposerverClient, rootRole: RootRole): Future[SignedPayload[TargetsRole]] =
    reposerverClient.targets().flatMap {
      case TargetsResponse(targets, checksum) =>
        val roleValidation = TufCrypto.payloadSignatureIsValid(rootRole, targets)

        roleValidation match {
          case Valid(_) if checksum.isDefined => writeTargets(targets, checksum.get).map(_ => targets).toFuture
          case Valid(_) => Future.failed(TargetsPullError("Did not receive valid role checksum from reposerver"))
          case Invalid(s) => Future.failed(TargetsPullError(s.toList.mkString(", ")))
        }
    }

  override def pushTargets(reposerverClient: ReposerverClient): Future[SignedPayload[TargetsRole]] =
    readSignedRole[TargetsRole].toFuture.flatMap { targets =>
      log.debug(s"pushing ${targets.asJson.spaces2}")

      for {
        checksum <- readChecksum[TargetsRole].toFuture
        _ <- reposerverClient.pushTargets(targets, checksum.some)
      } yield targets
    }

  def deleteTarget(filename: TargetFilename): Try[Path] = for {
    targetsRole <- readUnsignedRole[TargetsRole]
    newTargets <- if(targetsRole.targets.contains(filename))
      Success(targetsRole.copy(targets = targetsRole.targets - filename))
    else
      Failure(new IllegalArgumentException(s"Target $filename not found in unsigned targets.json"))
    path <- writeUnsignedRole(newTargets)
  } yield path

  def addTarget(targetFilename: TargetFilename, targetItem: ClientTargetItem): Try[Path] = {
    for {
      targetsRole <- readUnsignedRole[TargetsRole]
      newTargetRole = targetsRole.copy(targets = targetsRole.targets + (targetFilename -> targetItem))
      path <- writeUnsignedRole(newTargetRole)
    } yield path
  }

  override def signTargets(targetsKeys: Seq[KeyName], expiration: Instant => Instant, version: Option[Int] = None): Try[Path] =
    signRole[TargetsRole](version, targetsKeys, expiration)

  override def moveRootOffline(repoClient: ReposerverClient,
                               newRootName: KeyName,
                               oldRootName: KeyName,
                               oldKeyId: Option[KeyId],
                               newTargetsName: Option[KeyName],
                               rootExpireTime: Instant): Future[SignedPayload[RootRole]] = {
    assert(newTargetsName.isDefined, "new targets key name must be defined when moving root off line in tuf-reposerver")

    for {
      (newRootPubKey, newRootPrivKey) <- keyStorage.readKeyPair(newRootName).toFuture
      (newTargetsPubKey, _) <- keyStorage.readKeyPair(newTargetsName.get).toFuture
      oldRootRole <- repoClient.root().map(_.signed)
      _ <- ensureTargetsPulled(repoClient, oldRootRole)
      oldRootPubKeyId = oldKeyId.getOrElse(oldRootRole.roles(RoleType.ROOT).keyids.last)
      oldRootPubKey = oldRootRole.keys(oldRootPubKeyId)
      oldRootPrivKey <- deleteOrReadKey(repoClient, oldRootName, oldRootPubKeyId)
      _ <- keyStorage.writeKeys(oldRootName, oldRootPubKey, oldRootPrivKey).toFuture
      newRootRole = oldRootRole
        .withRoleKeys(RoleType.ROOT, threshold = 1, newRootPubKey)
        .withRoleKeys(RoleType.TARGETS, threshold = 1, newTargetsPubKey)
        .withVersion(oldRootRole.version + 1)
        .copy(expires = rootExpireTime)
      newRootSignature = TufCrypto.signPayload(newRootPrivKey, newRootRole.asJson).toClient(newRootPubKey.id)
      newRootOldSignature = TufCrypto.signPayload(oldRootPrivKey, newRootRole.asJson).toClient(oldRootPubKeyId)
      newSignedRoot = SignedPayload(Seq(newRootSignature, newRootOldSignature), newRootRole, newRootRole.asJson)
      _ = log.debug(s"pushing ${newSignedRoot.asJson.spaces2}")
      _ <- repoClient.pushSignedRoot(newSignedRoot)
      _ <- writeSignedRole(newSignedRoot).toFuture
    } yield newSignedRoot
  }

  override def pullTargets(reposerverClient: ReposerverClient): Future[SignedPayload[TargetsRole]] =
    reposerverClient.targets().map(_.targets)

  override def addTargetDelegation(name: DelegatedRoleName,
                                   keys: List[TufKey],
                                   delegatedPaths: List[DelegatedPathPattern], threshold: Int): Try[Path] = {
    readUnsignedRole[TargetsRole].flatMap { targets =>
      val existingDelegations = targets.delegations.getOrElse(Delegations(Map.empty, List.empty))
      val keyMap = keys.toKeyMap
      val newDelegation = ClientDataType.Delegation(name, keyMap.keys.toList, delegatedPaths)

      val newDelegations = existingDelegations.copy(
        keys = existingDelegations.keys ++ keyMap,
        roles = newDelegation :: existingDelegations.roles.filter(_.name != name)
      )

      val newTargets = targets.copy(delegations = newDelegations.some)

      writeUnsignedRole(newTargets)
    }

  }
}

class DirectorRepo(repoPath: Path)(implicit ec: ExecutionContext) extends TufRepo[DirectorClient](repoPath) {

  def ensureTargetsOnline(repoClient: TufServerClient, rootRole: RootRole): Future[Unit] = {
    val targetRoleKeys = rootRole.roles(RoleType.TARGETS)
    val keyPairs = targetRoleKeys.keyids.map { keyId =>
      repoClient.fetchKeyPair(keyId)
    }

    Future.sequence(keyPairs).map(_ => ())
  }

  override def moveRootOffline(repoClient: DirectorClient,
                               newRootName: KeyName,
                               oldRootName: KeyName,
                               oldKeyId: Option[KeyId],
                               newTargetsName: Option[KeyName],
                               rootExpireTime: Instant): Future[SignedPayload[RootRole]] = {
    assert(newTargetsName.isEmpty, "new targets key name must be empty for director")

    for {
      (newRootPubKey, newRootPrivKey) <- keyStorage.readKeyPair(newRootName).toFuture
      oldRootRole <- repoClient.root().map(_.signed)
      _ <- ensureTargetsOnline(repoClient, oldRootRole)
      oldRootPubKeyId = oldKeyId.getOrElse(oldRootRole.roles(RoleType.ROOT).keyids.last)
      oldRootPubKey = oldRootRole.keys(oldRootPubKeyId)
      oldRootPrivKey <- deleteOrReadKey(repoClient, oldRootName, oldRootPubKeyId)
      _ <- keyStorage.writeKeys(oldRootName, oldRootPubKey, oldRootPrivKey).toFuture
      newRootRole = oldRootRole
        .withRoleKeys(RoleType.ROOT, threshold = 1, newRootPubKey)
        .withVersion(oldRootRole.version + 1)
        .copy(expires = rootExpireTime)
      newRootSignature = TufCrypto.signPayload(newRootPrivKey, newRootRole.asJson).toClient(newRootPubKey.id)
      newRootOldSignature = TufCrypto.signPayload(oldRootPrivKey, newRootRole.asJson).toClient(oldRootPubKeyId)
      newSignedRoot = SignedPayload(Seq(newRootSignature, newRootOldSignature), newRootRole, newRootRole.asJson)
      _ = log.debug(s"pushing ${newSignedRoot.asJson.spaces2}")
      _ <- repoClient.pushSignedRoot(newSignedRoot)
      _ <- writeSignedRole(newSignedRoot).toFuture
    } yield newSignedRoot
  }

  override def initTargets(version: Int, expires: Instant): Try[Path] =
    Failure(CommandNotSupportedByRepositoryType(Director, "initTargets"))

  override def addTarget(targetFilename: TargetFilename, targetItem: ClientTargetItem): Try[Path] =
    Failure(CommandNotSupportedByRepositoryType(Director, "addTarget"))

  override def deleteTarget(filename: TargetFilename): Try[Path] =
    Failure(CommandNotSupportedByRepositoryType(Director, "deleteTarget"))

  override def signTargets(targetsKeys: Seq[KeyName], expiration: Instant => Instant, version: Option[Int]): Try[Path] =
    Failure(CommandNotSupportedByRepositoryType(Director, "signTargets"))

  override def pullVerifyTargets(client: DirectorClient, rootRole: RootRole): Future[SignedPayload[TargetsRole]] =
    Future.failed(CommandNotSupportedByRepositoryType(Director, "pullVerifyTargets"))

  override def pushTargets(client: DirectorClient): Future[SignedPayload[TargetsRole]] =
    Future.failed(CommandNotSupportedByRepositoryType(Director, "pushTargets"))

  override def pullTargets(client: DirectorClient): Future[SignedPayload[TargetsRole]] =
    Future.failed(CommandNotSupportedByRepositoryType(Director, "pullTargets"))

  override def addTargetDelegation(name: DelegatedRoleName, key: List[TufKey], delegatedPaths: List[DelegatedPathPattern], threshold: Int): Try[Path] =
    Failure(CommandNotSupportedByRepositoryType(Director, "addTargetDelegation"))
}
