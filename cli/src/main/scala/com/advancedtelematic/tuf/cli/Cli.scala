package com.advancedtelematic.tuf.cli

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.security.Security
import java.time.Instant
import java.time.temporal.ChronoUnit

import io.circe.syntax._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, HardwareIdentifier, KeyId, KeyType, TargetName, TargetVersion}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.slf4j.LoggerFactory
import com.advancedtelematic.libtuf.data.ClientCodecs._
import cats.syntax.option._
import eu.timepit.refined.api.Refined
import TryToFuture._
import com.advancedtelematic.libats.data.DataType.ValidChecksum
import com.advancedtelematic.tuf.cli.DataType.{KeyName, RepoName}
import com.advancedtelematic.tuf.cli.client.UserReposerverHttpClient

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
case object PullTargets extends Command
case object PushTargets extends Command
case object PushTargetsKey extends Command
case object Export extends Command
case object VerifyRoot extends Command

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
                  targetUri: URI = URI.create(""),
                  keySize: Int = 2048,
                  inputPath: Path = Paths.get("empty"),
                  exportPath: Option[Path] = None)

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
      .action { (_, c) =>
        c.copy(command = InitRepo)
      }
      .text("Initialize an empty repository")
      .children(
        opt[Path]("credentials")
          .abbr("c")
          .text("path to credentials file, treehub.json or credentials.zip")
          .required()
          .action { (path, c) =>
            c.copy(credentialsPath = path)
          },
        opt[KeyName]("target-key-name")
          .text("Target key name to look for in credentials.zip")
          .action { (name, c) =>
            c.copy(targetsKey = name)
          }
      )

    cmd("gen-keys")
      .action { (_, c) =>
        c.copy(command = GenKeys)
      }
      .text("Generate new offline keys")
      .children(
        opt[KeyName]("name").abbr("n").required().action { (name, c) =>
          c.copy(rootKey = name)
        },
        opt[KeyType]("type")
          .abbr("t")
          .action { (keyType, c) =>
            c.copy(keyType = keyType)
          }
          .text("key type, ec or rsa"),
        opt[Int]("keysize").text("defaults to 2048 for RSA keys").action {
          (keySize, c) =>
            c.copy(keySize = keySize)
        }
      )

    cmd("push-targets-key")
      .action { (_, c) =>
        c.copy(command = PushTargetsKey)
      }
      .children(
        opt[KeyName]("name").required().action { (arg, c) =>
          c.copy(targetsKey = arg)
        }
      )

    cmd("rotate")
      .action { (_, c) =>
        c.copy(command = Rotate)
      }
      .text("Download root.json, sign a new root.json with offline keys")
      .children(
        opt[KeyName]("new-root")
          .text("new root key to add to root.json, must exist")
          .required()
          .action { (keyName, c) =>
            c.copy(rootKey = keyName)
          },
        opt[KeyName]("new-targets")
          .text("new targets key to add to root.json, must exist")
          .required()
          .action { (keyName, c) =>
            c.copy(targetsKey = keyName)
          },
        opt[KeyName]("old-root-alias")
          .text(
            "old root key alias, the old root key will be saved under this name")
          .required()
          .action { (keyName, c) =>
            c.copy(oldRootKey = keyName)
          },
        opt[KeyId]("old-keyid")
          .text("key id to remove from root.json. This setting is optional and this app will try to use the last of the keys defined in the current root.json")
          .action { (keyId, c) =>
            c.copy(oldKeyId = keyId.some)
          }
      )

    cmd("targets")
      .action { (_, c) =>
        c.copy(command = InitTargets)
      }
      .children(
        cmd("init")
          .action { (_, c) =>
            c.copy(command = InitTargets)
          }
          .children(
            opt[Int]("version")
              .action((version, c) => c.copy(version = version))
              .required(),
            opt[Instant]("expires")
              .action((expires, c) => c.copy(expires = expires))
              .required()
          ),
        cmd("add")
          .action { (_, c) =>
            c.copy(command = AddTarget)
          }
          .children(
            opt[Int]("length")
              .required()
              .action { (arg, c) =>
                c.copy(length = arg)
              }
              .text("length in bytes"),
            opt[TargetName]("name")
              .required()
              .action { (arg, c) =>
                c.copy(targetName = arg.some)
              },
            opt[TargetVersion]("version")
              .required()
              .action { (arg, c) =>
                c.copy(targetVersion = arg.some)
              },
            opt[Refined[String, ValidChecksum]]("sha256")
              .required()
              .action { (arg, c) =>
                c.copy(checksum = arg.some)
              },
            opt[Seq[HardwareIdentifier]]("hardwareids")
              .required()
              .action { (arg, c) =>
                c.copy(hardwareIds = arg.toList)
              },
            opt[URI]("url")
              .required()
              .action { (arg, c) =>
                c.copy(targetUri = arg)
              }
          ),
        cmd("sign")
          .action { (_, c) =>
            c.copy(command = SignTargets)
          }
          .children(
            opt[KeyName]("key-name")
              .action { (arg, c) =>
                c.copy(targetsKey = arg)
              }
              .required()
          ),
        cmd("pull")
          .action { (_, c) =>
            c.copy(command = PullTargets)
          }.children(
            opt[KeyName]("root-key")
              .action{ (arg, c) =>
                c.copy(rootKey = arg)
              }
              .required()
          ),
        cmd("push").action { (_, c) =>
          c.copy(command = PushTargets)
        }
      )

    cmd("export-credentials")
      .text("Copy existing credentials.zip with treehub.json updated from repo")
      .action { (_, c) =>
        c.copy(command = Export)
      }
      .children(
        opt[KeyName]("target-key-name")
          .text("name of target keys (public and private) to export")
          .required()
          .action { (argc, c) =>
            c.copy(targetsKey = argc)
          },
        opt[Path]("to")
          .text("name of ZIP file to export to")
          .required()
          .action { (arg, c) =>
            c.copy(exportPath = Some(arg))
          }
      )

    cmd("get-targets")
      .text("get a repo targets, show on console")
      .hidden()
      .action { (_, c) =>
        c.copy(command = GetTargets)
      }

    cmd("verify-root")
      .text("verifies signatures for a signed root.json file")
      .hidden()
      .action { (_, c) => c.copy(command = VerifyRoot) }
      .children(
        opt[Path]('i', "in").action { (arg, c) => c.copy(inputPath = arg) }
      )
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

    lazy val tufRepo = new TufRepo(config.repoName, repoPath)

    lazy val repoServer = UserReposerverHttpClient.forRepo(tufRepo)

    val f: Future[_] = config.command match {
      case GenKeys =>
        tufRepo.genKeys(config.rootKey, config.keyType, config.keySize).toFuture

      case InitRepo =>
        tufRepo.init(config.credentialsPath, config.targetsKey).toFuture

      case Rotate =>
        repoServer
          .flatMap { client =>
            tufRepo.rotateRoot(client,
                               config.rootKey,
                               config.oldRootKey,
                               config.targetsKey,
                               config.oldKeyId)
          }
          .map(newRoot => log.info(newRoot.asJson.spaces2))

      case GetTargets =>
        repoServer
          .flatMap(_.targets())
          .map { case (targets,_) => log.info(targets.asJson.spaces2) }

      case InitTargets =>
        tufRepo
          .initTargets(config.version, config.expires)
          .map(p => log.info(s"Wrote empty targets to $p"))
          .toFuture

      case AddTarget =>
        tufRepo
          .addTarget(config.targetName.get,
                     config.targetVersion.get,
                     config.length,
                     config.checksum.get,
                     config.hardwareIds,
                     config.targetUri)
          .map(p => log.info(s"added target to $p"))
          .toFuture

      case SignTargets =>
        tufRepo
          .signTargets(config.targetsKey)
          .map(p => log.info(s"signed targets.json to $p"))
          .toFuture

      case PullTargets =>
        repoServer
          .flatMap { tufRepo.pullTargets(_, config.rootKey) }
          .map(_ => log.info("Pulled targets"))

      case PushTargets =>
        repoServer
          .flatMap(tufRepo.pushTargets)
          .map(_ => log.info("Pushed targets"))

      case PushTargetsKey =>
        repoServer
          .flatMap(client => tufRepo.pushTargetsKey(client, config.targetsKey))
          .map(key => log.info(s"Pushed key ${key.id} to server"))

      case Export =>
        tufRepo.export(config.targetsKey, config.exportPath.get).toFuture

      case VerifyRoot =>
        CliUtil.verifyRootFile(config.inputPath)

      case Help =>
        parser.showUsage()
        Future.successful(())
    }

    Await.result(f, Duration.Inf)
  }
}
