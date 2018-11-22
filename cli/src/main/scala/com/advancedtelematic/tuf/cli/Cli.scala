package com.advancedtelematic.tuf.cli

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.security.Security
import java.time.Instant
import java.time.temporal.ChronoUnit

import cats.syntax.either._
import cats.syntax.option._
import ch.qos.logback.classic.{Level, Logger}
import com.advancedtelematic.libats.data.DataType.ValidChecksum
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, KeyId, KeyType, TargetFilename, TargetFormat, TargetName, TargetVersion}
import com.advancedtelematic.libtuf.http.TufServerHttpClient.RoleChecksumNotValid
import com.advancedtelematic.libtuf.http.{DirectorClient, ReposerverClient}
import com.advancedtelematic.tuf.cli.Commands._
import com.advancedtelematic.tuf.cli.DataType._
import com.advancedtelematic.tuf.cli.Errors.CliArgumentsException
import com.advancedtelematic.tuf.cli.GenericOptionDefOps._
import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.tuf.cli.http.TufRepoCliClient
import com.advancedtelematic.tuf.cli.http.TufRepoCliClient._
import com.advancedtelematic.tuf.cli.repo.{CliKeyStorage, DirectorRepo, RepoServerRepo, TufRepo}
import eu.timepit.refined.api.Refined
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.slf4j.LoggerFactory
import scopt.{OptionDef, OptionParser}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

case class Config(command: Command,
                  home: Path = Paths.get("tuf"),
                  credentialsPath: Path = Paths.get("credentials.zip"),
                  repoType: Option[TufServerType] = RepoServer.some,
                  repoName: RepoName = RepoName("default-repo"),
                  rootKey: KeyName = KeyName("default-key"),
                  keyType: KeyType = KeyType.default,
                  oldRootKey: KeyName = KeyName("default-key"),
                  keyNames: List[KeyName]= List.empty,
                  keyIds: List[KeyId]= List.empty,
                  oldKeyId: Option[KeyId] = None,
                  version: Option[Int] = None,
                  expires: Instant = Instant.now().plus(1, ChronoUnit.DAYS),
                  length: Int = -1,
                  targetFilename: Option[TargetFilename] = None,
                  targetName: Option[TargetName] = None,
                  targetFormat: TargetFormat = TargetFormat.BINARY,
                  targetVersion: Option[TargetVersion] = None,
                  checksum: Option[Refined[String, ValidChecksum]] = None,
                  hardwareIds: List[HardwareIdentifier] = List.empty,
                  targetUri: Option[URI] = None,
                  keySize: Option[Int] = None,
                  userKeysPath: Option[Path] = None,
                  inputPath: Path = Paths.get("empty"),
                  exportPath: Path = Paths.get(""),
                  force: Boolean = false,
                  reposerverUrl: Option[URI] = None,
                  verbose: Boolean = false)

object Cli extends App with VersionInfo {

  import CliReads._

  Security.addProvider(new BouncyCastleProvider)

  private lazy val log = LoggerFactory.getLogger(this.getClass)

  val PROGRAM_NAME = "garage-sign"

  lazy val repoNameOpt: OptionParser[Config] => OptionDef[RepoName, Config] = { parser =>
    parser
      .opt[RepoName]("repo").abbr("r").required()
      .toConfigParam('repoName)
      .text("name of the repository, this should be a directory in your tuf-home and can be initialized with the init command")
  }

  lazy val manyKeyNamesOpt: OptionParser[Config] => OptionDef[KeyName, Config] = { parser =>
    parser
      .opt[KeyName]("key-name").abbr("k")
      .unbounded()
      .required()
      .action { (arg, config) => config.copy(keyNames = arg :: config.keyNames) }
  }

  lazy val keysPathOpt: OptionParser[Config] => OptionDef[Path, Config] = { parser =>
    parser
      .opt[Path]("keys-path").abbr("p")
      .toConfigOptionParam('userKeysPath)
      .text("Path where this executable will look for keys, by default it's the `user-keys` directory in `home-dir`")
  }

  lazy val parser = new scopt.OptionParser[Config](PROGRAM_NAME) {
    head(projectName, projectVersion)

    help("help").text("prints this usage text")

    version("version")

    opt[Path]("home-dir").abbr("h").toConfigParam('home)

    opt[Unit]("verbose").action { (_, c) =>
      c.copy(verbose = true)
    }

    version("version")

    cmd("keys").children(
      keysPathOpt(this),
      cmd("gen")
        .toCommand(GenUserKey)
        .children(
          manyKeyNamesOpt(this).maxOccurs(1)
        )
    ).text("manage keys not associated with a specific repository")

    cmd("delegations").children(
      cmd("init")
        .toCommand(CreateDelegation)
        .text("Output empty delegation metadata, which can be edited and signed"),
      cmd("sign")
        .toCommand(SignDelegation)
        .children(
          manyKeyNamesOpt(this),
          opt[Path]("input")
            .abbr("i").required()
            .toConfigParam('inputPath)
        )
      //      cmd("push")
      //        .text("push a delegation to the server") // TODO: Needs credentials.zip

      //      pull ?
    )

    cmd("init")
      .toCommand(InitRepo)
      .text("Initialize an empty repository")
      .children(
        repoNameOpt(this),
        opt[URI]("reposerver").toConfigOptionParam('reposerverUrl),
        opt[Path]("credentials")
          .abbr("c")
          .toConfigParam('credentialsPath)
          .text("path to credentials file, credentials.zip")
          .required(),
        opt[TufServerType]("servertype")
          .abbr("t")
          .text("repo server type, 'reposerver' or 'director'")
          .toConfigOptionParam('repoType)
          .optional()
      )

    cmd("key").children(
      cmd("generate")
        .toCommand(GenRepoKeys)
        .text("Generate a new key")
        .children(
          repoNameOpt(this),
          opt[KeyName]("name").abbr("n").required()
            .toConfigParam('rootKey),
          opt[KeyType]("type")
            .abbr("t")
            .toConfigParam('keyType)
            .text("key type, ed25519 or rsa"),
          opt[Int]("keysize")
            .action { (keySize, c) => c.copy(keySize = keySize.some) }
        )
    )

    cmd("move-offline")
      .toCommand(MoveOffline)
      .text(
        """Move root keys to offline keys
          | Deletes online root keys from director or root and target keys for from repo server
          | Downloads and signs a new root.json with new offline keys
          | Push the new root
        """.stripMargin)
      .children(
        repoNameOpt(this),
        opt[KeyName]("new-root")
          .text("new root key to add to root.json, must exist")
          .required()
          .toConfigParam('rootKey),
        opt[KeyName]("new-targets")
          .text("new targets key to add to root.json, must exist (only for repo server)")
          .action { (keyName: KeyName, c) => c.copy(keyNames = List(keyName)) },
        opt[KeyName]("old-root-alias")
          .text("old root key alias, the old root key will be saved under this name")
          .required()
          .toConfigParam('oldRootKey),
        opt[KeyId]("old-keyid")
          .text("key id to remove from root.json. This setting is optional and this app will try to use the last of the keys defined in the current root.json")
          .toConfigOptionParam('oldKeyId)
      )

    cmd("root")
      .children(
        repoNameOpt(this),
        cmd("pull")
          .toCommand(PullRoot)
          .children(
            opt[Unit]("force")
              .action { (_, c) => c.copy(force = true) }
              .text("Skip validation of remote root.json against local root.json")
              .hidden()
          ),
        cmd("push")
          .toCommand(PushRoot),
        cmd("key")
          .children(
            cmd("add")
              .toCommand(AddRootKey)
              .children(
                manyKeyNamesOpt(this)
              ),
            cmd("remove")
              .toCommand(RemoveRootKey)
              .children(
                manyKeyNamesOpt(this),
                opt[KeyId]("key-id")
                  .unbounded()
                  .action { (arg, c) => c.copy(keyIds = arg :: c.keyIds) },
              )
          ),
        cmd("sign")
          .toCommand(SignRoot)
          .children(
            manyKeyNamesOpt(this)
          )
      )

    cmd("targets")
      .text("(only for repo server)")
      .toCommand(InitTargets)
      .children(
        repoNameOpt(this),
        cmd("init")
          .toCommand(InitTargets)
          .children(
            opt[Int]("version")
              .toConfigOptionParam('version)
              .required(),
            opt[Instant]("expires")
              .toConfigParam('expires)
              .required()
          ),
        cmd("delete")
          .toCommand(DeleteTarget)
          .children(
            opt[TargetFilename]("filename")
              .required()
              .toConfigOptionParam('targetFilename)
          ),
        cmd("add")
          .toCommand(AddTarget)
          .children(
            opt[Int]("length")
              .required()
              .toConfigParam('length)
              .text("length in bytes"),
            opt[TargetName]("name")
              .required()
              .toConfigOptionParam('targetName),
            opt[TargetVersion]("version")
              .required()
              .toConfigOptionParam('targetVersion),
            opt[TargetFormat]("format")
              .optional()
              .text("target format [ostree|binary]")
              .toConfigParam('targetFormat),
            opt[Refined[String, ValidChecksum]]("sha256")
              .required()
              .toConfigOptionParam('checksum),
            opt[List[HardwareIdentifier]]("hardwareids")
              .required()
              .toConfigParam('hardwareIds),
            opt[URI]("url")
              .toConfigOptionParam('targetUri)
          ),
        cmd("sign")
          .toCommand(SignTargets)
          .children(
            opt[KeyName]("key-name")
              .action { (arg, c) => c.copy(keyNames = List(arg)) }
              .required(),
            opt[Int]("version")
              .text("Ignore unsigned role version and use <version> instead")
              .toConfigOptionParam('version)
          ),
        cmd("pull")
          .toCommand(PullTargets),
        cmd("push")
          .toCommand(PushTargets)
          .text("""push latest targets.json to server This will fail with exit code 2 if the latest `pull`
                  |was too long ago and did not pull the latest targets.json on the server.""".stripMargin),
        cmd("delegations")
          .text("Manage a repository targets.json delegated targets")
          .children(
            //
            //      cmd("add")
            //        .text("add a new delegation to existing targets.json") // TODO: Needs to be on repo level?, needs credentials.zip

            //      remove ?
          )
      )

    cmd("export-credentials")
      .text("Export settings and keys to credentials.zip")
      .toCommand(ExportRepository)
      .children(
        repoNameOpt(this),
        manyKeyNamesOpt(this).text("name of target keys (public and private) to export"),
        opt[Path]("to")
          .text("name of ZIP file to export to")
          .required()
          .toConfigParam('exportPath)
      )

    cmd("get-targets")
      .toCommand(GetTargets)
      .text("get a repo targets, show on console")
      .hidden()
      .children(repoNameOpt(this))

    cmd("verify-root")
      .toCommand(VerifyRoot)
      .text("verifies signatures for a signed root.json file")
      .hidden()
      .children(
        repoNameOpt(this),
        opt[Path]('i', "input").toConfigParam('inputPath)
      )

    checkConfig { c =>
      c.command match {
        case RemoveRootKey if c.keyIds.isEmpty && c.keyNames.isEmpty =>
          "To remove a root key you need to specify at least one key id or key name".asLeft
        case _ =>
          Right(())
      }
    }
  }

  def validateRepoTypeCliArguments(repoPath: Path, config: Config): Either[String, TufServerType] = {
    val repoType =
      if (config.command == InitRepo)
      // a required param for this command
        config.repoType.get
      else
        TufRepo.readConfigFile(repoPath).map(_.repoServerType).getOrElse(RepoServer)

    if((repoType == RepoServer) && config.keyNames.isEmpty)
      "Missing argument: target key name required to move repo server keyname offline".asLeft

    repoType.asRight
  }

  val default = Config(command = Help)

  parser.parse(args, default) match {
    case Some(c) =>
      if (c.verbose) {
        setLoggingLevel(Level.DEBUG)
      }

      execute(c)
    case None =>
      sys.exit(-1)
  }

  def buildCommandHandler(repoPath: Path, config: Config)(repoType: TufServerType): Config => Future[Unit] = {
    val userKeyPath = config.userKeysPath.getOrElse(repoPath.resolve("user-keys"))
    val userKeyStorage = CliKeyStorage.forUser(userKeyPath)

    repoType match {
      case Director =>
        val tufRepo = new DirectorRepo(config.repoName, repoPath)
        lazy val repoServer = TufRepoCliClient.forRepo[DirectorClient](tufRepo)

        CommandHandler.handle(tufRepo, userKeyStorage, repoServer)
      case RepoServer =>
        val tufRepo = new RepoServerRepo(config.repoName, repoPath)
        val repoServer = TufRepoCliClient.forRepo[ReposerverClient](tufRepo)

        CommandHandler.handle(tufRepo, userKeyStorage, repoServer)
    }
  }

  def execute(config: Config): Unit = {
    Files.createDirectories(config.home)
    val repoPath = config.home.resolve(config.repoName.value)
    val repoTypeE = validateRepoTypeCliArguments(repoPath, config)
    val commandHandler = repoTypeE.map(buildCommandHandler(repoPath, config)).leftMap(CliArgumentsException).toTry.toFuture

    try
      Await.result(commandHandler.flatMap { handler => handler(config) } , Duration.Inf)
    catch {
      case ex if CliHelp.explainError.isDefinedAt(ex) =>
        CliHelp.explainError(ex)
        sys.exit(1)

      case ex @ RoleChecksumNotValid =>
        log.error("Could not push targets", ex)
        sys.exit(2)

      case ex: Throwable =>
        System.err.println("An error occurred", ex)
        sys.exit(3)
    }
  }

  def setLoggingLevel(level: Level): Unit = {
    val logger: Logger = LoggerFactory.getLogger("com.advancedtelematic").asInstanceOf[Logger]
    logger.setLevel(level)
  }
}
