package com.advancedtelematic.tuf.cli

import java.net.URI
import java.nio.file.{Path, Paths}
import java.security.Security
import java.time.Instant
import java.time.temporal.ChronoUnit

import cats.syntax.either._
import cats.syntax.option._
import com.advancedtelematic.libats.data.DataType.ValidChecksum
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, HardwareIdentifier, KeyId, KeyType, TargetFilename, TargetFormat, TargetName, TargetVersion}
import com.advancedtelematic.libtuf.reposerver.UserTufServerClient.RoleChecksumNotValid
import com.advancedtelematic.libtuf.reposerver.{UserDirectorHttpClient, UserReposerverClient}
import com.advancedtelematic.tuf.cli.Commands._
import com.advancedtelematic.tuf.cli.DataType._
import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.tuf.cli.client.{UserDirectorHttpClient, UserReposerverHttpClient}
import com.advancedtelematic.tuf.cli.repo.{DirectorRepo, RepoServerRepo, TufRepo}
import eu.timepit.refined.api.Refined
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

case class Config(command: Command,
                  home: Path = Paths.get("tuf"),
                  credentialsPath: Path = Paths.get("credentials.zip"),
                  repoName: RepoName = RepoName("default-repo"),
                  rootKey: KeyName = KeyName("default-key"),
                  keyType: KeyType = Ed25519KeyType,
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
                  inputPath: Path = Paths.get("empty"),
                  exportPath: Path = Paths.get(""),
                  force: Boolean = false,
                  reposerverUrl: Option[URI] = None)


object Cli extends App with VersionInfo {

  import CliReads._

  Security.addProvider(new BouncyCastleProvider)

  val PROGRAM_NAME = "garage-sign"

  lazy private val log = LoggerFactory.getLogger(this.getClass)

  lazy val parser = new scopt.OptionParser[Config](PROGRAM_NAME) {
    head(projectName, projectVersion)

    help("help").text("prints this usage text")

    opt[Path]("home-dir").abbr("h").action { (file, c) =>
      c.copy(home = file)
    }

    opt[RepoName]("repo").abbr("r").required().action { (name, c) =>
      c.copy(repoName = name)
    }
    .text("required for all subcommands")

    version("version")

    cmd("init")
      .action { (_, c) =>
        c.copy(command = InitRepo)
      }
      .text("Initialize an empty repository")
      .children(
        opt[URI]("reposerver").action { (arg, c) =>
          c.copy(reposerverUrl = arg.some)
        },
        opt[Path]("credentials")
          .abbr("c")
          .action { (path, c) =>
            c.copy(credentialsPath = path)
          }
          .text("path to credentials file, credentials.zip")
          .required()
      )

    cmd("key").children(
      cmd("generate")
        .action { (_, c) => c.copy(command = GenKeys) }
        .text("Generate a new key")
        .children(
          opt[KeyName]("name").abbr("n").required().action { (name, c) =>
            c.copy(rootKey = name)
          },
          opt[KeyType]("type")
            .abbr("t")
            .action { (keyType, c) =>
              c.copy(keyType = keyType)
            }
            .text("key type, ed25519 or rsa"),
          opt[Int]("keysize")
            .action { (keySize, c) => c.copy(keySize = keySize.some) }
        )
    )

    cmd("move-offline")
      .action { (_, c) =>
        c.copy(command = MoveOffline)
      }
      .text(
        """Move root keys to offline keys
          | Deletes online root and target keys from server
          | Downloads and signs a new root.json with new offline keys
          | Push the new root
        """.stripMargin)
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
          .action { (keyName: KeyName, c) =>
            c.copy(keyNames = List(keyName))
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

    cmd("root")
      .children(
        cmd("pull")
          .action { (_, c) => c.copy(command = PullRoot) }
          .children(
            opt[Unit]("force")
              .action { (_, c) => c.copy(force = true) }
              .text("Skip validation of remote root.json against local root.json")
              .hidden()
          ),
        cmd("push")
          .action { (_, c) => c.copy(command = PushRoot) },
        cmd("key")
          .children(
            cmd("add")
              .action { (_, c) => c.copy(command = AddRootKey) }
              .children(
                opt[KeyName]("key-name")
                  .unbounded()
                  .required()
                  .action { (arg, c) => c.copy(keyNames = arg :: c.keyNames) }
              ),
            cmd("remove")
              .action { (_, c) => c.copy(command = RemoveRootKey) }
              .children(
                opt[KeyName]("key-name")
                  .unbounded()
                  .action { (arg, c) => c.copy(keyNames = arg :: c.keyNames) },
                opt[KeyId]("key-id")
                  .unbounded()
                  .action { (arg, c) => c.copy(keyIds = arg :: c.keyIds) },
              )
          ),
        cmd("sign")
          .action { (_, c) => c.copy(command = SignRoot) }
          .children(
            opt[KeyName]("key-name")
              .unbounded()
              .action { (arg, c) =>
                c.copy(keyNames = arg :: c.keyNames)
              }
              .required()
          )
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
              .action((version, c) => c.copy(version = version.some))
              .required(),
            opt[Instant]("expires")
              .action((expires, c) => c.copy(expires = expires))
              .required()
          ),
        cmd("delete")
          .action { (_, c) =>
            c.copy(command = DeleteTarget)
          }.children(
          opt[TargetFilename]("filename")
            .required()
            .action { (arg, c) =>
              c.copy(targetFilename = arg.some)
            }
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
            opt[TargetFormat]("format")
              .optional()
              .text("target format [ostree|binary]")
              .action { (arg, c) =>
                c.copy(targetFormat = arg)
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
              .action { (arg, c) =>
                c.copy(targetUri = arg.some)
              }
          ),
        cmd("sign")
          .action { (_, c) =>
            c.copy(command = SignTargets)
          }
          .children(
            opt[KeyName]("key-name")
              .action { (arg, c) =>
                c.copy(keyNames = List(arg))
              }
              .required(),
            opt[Int]("version")
              .text("Ignore unsigned role version and use <version> instead")
              .action { (arg, c) => c.copy(version = arg.some) }
          ),
        cmd("pull")
          .action { (_, c) =>
            c.copy(command = PullTargets)
          },
        cmd("push")
          .text("""push latest targets.json to server This will fail with exit code 2 if the latest `pull`
                  |was too long ago and did not pull the latest targets.json on the server.""".stripMargin)
          .action { (_, c) =>
            c.copy(command = PushTargets)
          }
      )

    cmd("export-credentials")
      .text("Export settings and keys to credentials.zip")
      .action { (_, c) =>
        c.copy(command = Export)
      }
      .children(
        opt[KeyName]("target-key-name")
          .text("name of target keys (public and private) to export")
          .required()
          .action { (argc, c) =>
            c.copy(keyNames = List(argc))
          },
        opt[Path]("to")
          .text("name of ZIP file to export to")
          .required()
          .action { (arg, c) =>
            c.copy(exportPath = arg)
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

    checkConfig { c =>
      c.command match {
        case RemoveRootKey if c.keyIds.isEmpty && c.keyNames.isEmpty =>
          "To remove a root key you need to specify at least one key id or key name".asLeft
        case _ =>
          Right(())
      }
    }
  }

  val default = Config(command = Help)

  parser.parse(args, default) match {
    case Some(c) =>
      execute(c)
    case None =>
      sys.exit(-1)
  }

  def execute(config: Config): Unit = {
    import ExecutionContext.Implicits.global

    val repoPath = config.home.resolve(config.repoName.value)

    val tufRepo: RepoServerRepo = new RepoServerRepo(config.repoName, repoPath)
    val repoServer: Future[UserReposerverClient] = UserReposerverHttpClient.forRepo(tufRepo)
    val executor = new ReposerverExecutor(config, repoServer, tufRepo)

    try
      Await.result(executor.dispatch(config.command).recoverWith(CliHelp.explainErrorHandler), Duration.Inf)
    catch {
      case ex @ RoleChecksumNotValid =>
        log.error("Could not push targets", ex)
        sys.exit(2)
      case ex: Throwable => // Already logged
        log.debug(ex.getMessage)
        sys.exit(3)
    }
  }
}
