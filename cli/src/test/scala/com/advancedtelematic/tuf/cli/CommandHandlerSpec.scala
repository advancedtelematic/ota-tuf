package com.advancedtelematic.tuf.cli

import java.nio.file.Files

import cats.data.Validated.Valid
import com.advancedtelematic.libtuf.data.ClientDataType.TargetsRole
import com.advancedtelematic.libtuf.data.TufDataType.{KeyType, SignedPayload}
import com.advancedtelematic.tuf.cli.Commands.{GenUserKey, Help, SignDelegation}
import com.advancedtelematic.tuf.cli.DataType.KeyName
import com.advancedtelematic.tuf.cli.repo.{CliKeyStorage, RepoServerRepo}
import com.advancedtelematic.tuf.cli.util.TufRepoInitializerUtil._
import com.advancedtelematic.tuf.cli.util.{CliSpec, FakeReposerverTufServerClient, KeyTypeSpecSupport}
import org.scalatest.Inspectors
import io.circe.jawn
import cats.syntax.either._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CommandHandlerSpec extends CliSpec with KeyTypeSpecSupport with Inspectors {

  val defaultConfig = Config(Help)

  lazy val tufRepo = initRepo[RepoServerRepo]()

  lazy val userKeyDir = Files.createTempDirectory("user-keys")

  lazy val userKeyStorage = CliKeyStorage.forUser(userKeyDir)

  lazy val client = FakeReposerverTufServerClient(KeyType.default)

  lazy val handler: Config => Future[Unit] = CommandHandler.handle(tufRepo, userKeyStorage, Future.successful(client))

  keyTypeTest("generates a key using the provided user key storage") { keyType =>
    val keyName01 = KeyName("mykey01")
    val keyName02 = KeyName("mykey02")
    val config = Config(GenUserKey, keyNames = List(keyName01, keyName02), keyType = keyType)

    handler(config).futureValue

    forAll(List(keyName01, keyName02)) { keyName =>
      val (pub, priv) = userKeyStorage.readKeyPair(keyName).get
      pub.keytype shouldBe keyType
      priv.keytype shouldBe keyType
    }
  }

  test("signs a payload with the provided keys") {
    val keyName01 = KeyName("mykey01")
    val keyName02 = KeyName("mykey02")

    val in = Files.createTempFile("in", ".json")
    val out = Files.createTempFile("out", ".json")

    Delegations.writeNew(in).get

    // TODO:SM Using exportPath = out, not valid in Cli.scala
    val config = Config(SignDelegation, keyNames = List(keyName01, keyName02), inputPath = in, exportPath = out)

    handler(config).futureValue

    val signedPayload = jawn.parseFile(out.toFile).flatMap(_.as[SignedPayload[TargetsRole]]).valueOr(throw _)

    val keys = List(keyName01, keyName02).map { keyName =>
      userKeyStorage.readKeyPair(keyName).get
    }.map { case (pub, _) => pub.id -> pub }.toMap

    TufCrypto.payloadSignatureIsValid(keys, threshold = 2, signedPayload = signedPayload) shouldBe a[Valid[_]]
  }
}
