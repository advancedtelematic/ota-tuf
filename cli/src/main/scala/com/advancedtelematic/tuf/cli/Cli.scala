package com.advancedtelematic.tuf.cli

import java.nio.file.{Files, Path, Paths}
import java.security.Security
import java.time.Instant
import java.time.temporal.ChronoUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.util.FastFuture
import akka.stream.{ActorMaterializer, Materializer}
import io.circe.syntax._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, HardwareIdentifier, KeyId, KeyType, TargetName, TargetVersion}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.slf4j.LoggerFactory
import com.advancedtelematic.libtuf.data.ClientCodecs._
import cats.syntax.option._
import eu.timepit.refined.api.Refined
import TryToFuture._
import com.advancedtelematic.libats.messaging_datatype.DataType.ValidChecksum
import com.advancedtelematic.tuf.cli.DataType.{KeyName, RepoName}
import com.advancedtelematic.tuf.cli.client.UserReposerverHtttpClient

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

sealed trait Command
case object Help extends Command
case object GenKeys extends Command
case object InitRepo extends Command
case object Rotate extends Command
case object GetTargets extends Command
case object InitTargets extends Command
case object AddTarget extends Command
case object SignTargets extends Command
case object PushTargets extends Command
case object PushTargetsKey extends Command


case class Config(command: Command,
                  home: Path = Paths.get("tuf"),
                  credentialsPath: Path = Paths.get("treehub.json"),
                  repoName: RepoName = RepoName("default-repo"),
                  rootKey: KeyName = KeyName("default-key"),
                  keyType: KeyType = EdKeyType,
                  oldRootKey: KeyName = KeyName("default-key"),
                  targetsKey: KeyName = KeyName("default-key"),
                  oldKeyId: Option[KeyId] = None,
                  version: Int = 1,
                  expires: Instant = Instant.now().plus(1, ChronoUnit.DAYS),
                  length: Int = -1,
                  targetName: Option[TargetName] = None,
                  targetVersion: Option[TargetVersion] = None,
                  checksum: Option[Refined[String, ValidChecksum]] = None,
                  hardwareIds: List[HardwareIdentifier] = List.empty,
                  targetUri: Uri = Uri.Empty,
                  keySize: Int = 2048)

object Cli extends App with VersionInfo {
  import CliReads._

  Security.addProvider(new BouncyCastleProvider)

  val PROGRAM_NAME = "garage-sign"

  import ExecutionContext.Implicits.global

  lazy private val log = LoggerFactory.getLogger(this.getClass)

  val parser = new scopt.OptionParser[Config](PROGRAM_NAME) {
    head(projectName, projectVersion)

    help("help").text("prints this usage text")

    opt[Path]("home-dir").abbr("h").action { (file, c) =>
      c.copy(home = file)
    }

    opt[RepoName]("repo").abbr("r").required().action { (name, c) =>
      c.copy(repoName = name)
    }

    cmd("init")
      .action { (_, c) => c.copy(command = InitRepo) }
      .text("Initialize an empty repository")
      .children(
        opt[Path]("credentials").abbr("c")
          .text("path to credentials file, usually treehub.json")
          .required()
          .action { (path, c) =>
            c.copy(credentialsPath = path)
          }
      )

    cmd("gen-keys")
      .action { (_, c) => c.copy(command = GenKeys) }
      .text("Generate new offline keys")
      .children(
        opt[KeyName]("name").abbr("n").required().action { (name, c) =>
          c.copy(rootKey = name)
        },
        opt[KeyType]("type").abbr("t").action { (keyType, c) =>
          c.copy(keyType = keyType)
        }.text("key type, ec or rsa")
        ,
        opt[Int]("keysize").text("defaults to 2048 for RSA keys").action { (keySize, c) =>
          c.copy(keySize = keySize)
        }
      )

    cmd("push-targets-key").action { (_, c) => c.copy(command = PushTargetsKey) }
      .children(
        opt[KeyName]("name").required().action { (arg, c) => c.copy(targetsKey = arg) }
      )

    cmd("rotate").action { (_, c) => c.copy(command = Rotate) }
      .text("Download root.json, sign a new root.json with offline keys")
      .children(
        opt[KeyName]("new-root")
          .text("new root key to add to root.json, must exist")
          .required().action { (keyName, c) => c.copy(rootKey = keyName) },
        opt[KeyName]("new-targets")
          .text("new targets key to add to root.json, must exist")
          .required().action { (keyName, c) => c.copy(targetsKey = keyName) },
        opt[KeyName]("old-root-alias")
          .text("old root key alias, the old root key will be saved under this name")
          .required().action { (keyName, c) => c.copy(oldRootKey = keyName) },
        opt[KeyId]("old-keyid")
          .text("key id to remove from root.json. This setting is optional and this app will try to use the last of the keys defined in the current root.json")
          .action { (keyId, c) => c.copy(oldKeyId = keyId.some) }
      )

    cmd("targets").action { (_, c) => c.copy(command = InitTargets ) }
      .children(
        cmd("init").action { (_, c) => c.copy(command = InitTargets ) }
          .children(
            opt[Int]("version")
              .action( (version, c) => c.copy(version = version) )
              .required(),
            opt[Instant]("expires")
              .action( (expires, c) => c.copy(expires = expires) )
              .required()
          ),
        cmd("add").action { (_, c) => c.copy(command = AddTarget) }
          .children(
            opt[Int]("length")
              .required()
              .action  { (arg, c) => c.copy(length = arg) }
              .text("length in bytes"),
            opt[TargetName]("name")
              .required()
              .action { (arg, c) => c.copy(targetName = arg.some) },
            opt[TargetVersion]("version")
              .required()
              .action { (arg, c) => c.copy(targetVersion = arg.some) },
            opt[Refined[String, ValidChecksum]]("sha256")
              .required()
              .action { (arg, c) => c.copy(checksum = arg.some) },
            opt[Seq[HardwareIdentifier]]("hardwareids")
              .required()
              .action { (arg, c) => c.copy(hardwareIds = arg.toList) },
            opt[Uri]("url")
              .required()
              .action { (arg, c) => c.copy(targetUri = arg) }
          ),
        cmd("sign").action { (_, c) => c.copy(command = SignTargets) }
          .children(
            opt[KeyName]("key-name").action { (arg, c) => c.copy(targetsKey = arg) }
              .required()
          ),
        cmd("push").action { (_, c) => c.copy(command = PushTargets) }
      )

    cmd("get-targets")
      .text("get a repo targets, show on console")
      .hidden()
      .action { (_, c) => c.copy(command = GetTargets) }
  }

  val default = Config(command = Help)

  parser.parse(args, default) match {
    case Some(c) =>
      execute(c)
    case None =>
      sys.exit(-1)
  }

  def execute(config: Config): Unit = {
    Files.createDirectories(config.home)

    lazy val repoPath = config.home.resolve(config.repoName.value)

    implicit lazy val system = ActorSystem(PROGRAM_NAME)
    implicit lazy val mat = ActorMaterializer()

    lazy val tufRepo = new TufRepo(config.repoName, repoPath)

    lazy val repoServer = UserReposerverHtttpClient.forRepo(tufRepo)

    val f: Future[_] = config.command match {
      case GenKeys =>
        tufRepo.genKeys(config.rootKey, config.keyType, config.keySize).toFuture

      case InitRepo =>
        tufRepo.init(config.credentialsPath).toFuture

      case Rotate =>
        repoServer.flatMap { client =>
          tufRepo.rotateRoot(client, config.rootKey, config.oldRootKey, config.targetsKey, config.oldKeyId)
        }.map(newRoot => log.info(newRoot.asJson.spaces2))

      case GetTargets =>
        repoServer
          .flatMap(_.targets())
          .map(targets => log.info(targets.asJson.spaces2))

      case InitTargets =>
        tufRepo.initTargets(config.version, config.expires)
          .map(p => log.info(s"Wrote empty targets to $p"))
          .toFuture

      case AddTarget =>
        tufRepo.addTarget(config.targetName.get, config.targetVersion.get, config.length, config.checksum.get, config.hardwareIds, config.targetUri)
          .map(p => log.info(s"added target to $p"))
          .toFuture

      case SignTargets =>
        tufRepo.signTargets(config.targetsKey)
          .map(p => log.info(s"signed targets.json to $p"))
          .toFuture

      case PushTargets =>
        repoServer
          .flatMap(tufRepo.pushTargets)
          .map(_ => log.info("Pushed targets"))

      case PushTargetsKey =>
        repoServer
          .flatMap(client => tufRepo.pushTargetsKey(client, config.targetsKey))
          .map(key => log.info(s"Pushed key ${key.id} to server"))

      case Help =>
        parser.showUsage()
        FastFuture.successful(())
    }

    f.onComplete(_ => system.terminate())

    Await.result(f, Duration.Inf)
  }
}
