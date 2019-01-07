package com.advancedtelematic.tuf.cli

import java.io.FileOutputStream
import java.nio.file.Files
import java.time.Instant

import com.advancedtelematic.libtuf.data.ValidatedString._
import com.advancedtelematic.libtuf.data.ClientDataType.DelegatedRoleName._
import com.advancedtelematic.libtuf.data.ClientDataType.DelegatedPathPattern._
import cats.data.Validated.Valid
import cats.syntax.either._
import cats.syntax.option._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType
import com.advancedtelematic.libtuf.data.ClientDataType.{DelegatedPathPattern, DelegatedRoleName, TargetsRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{KeyType, SignedPayload}
import com.advancedtelematic.tuf.cli.Commands._
import com.advancedtelematic.tuf.cli.DataType.KeyName
import com.advancedtelematic.tuf.cli.repo.{CliKeyStorage, RepoServerRepo}
import com.advancedtelematic.tuf.cli.util.TufRepoInitializerUtil._
import com.advancedtelematic.tuf.cli.util.{CliSpec, FakeReposerverTufServerClient, KeyTypeSpecSupport}
import io.circe.jawn
import io.circe.syntax._
import org.scalatest.Inspectors

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CommandHandlerSpec extends CliSpec with KeyTypeSpecSupport with Inspectors {

  val defaultConfig = Config(Help)

  lazy val tufRepo = initRepo[RepoServerRepo]()

  lazy val userKeyDir = Files.createTempDirectory("user-keys")

  lazy val userKeyStorage = CliKeyStorage.forUser(userKeyDir)

  lazy val client = FakeReposerverTufServerClient(KeyType.default)

  lazy val handler: Config => Future[Unit] = config =>
    CommandHandler.handle(tufRepo, Future.successful(client), Future.successful(client), userKeyStorage, config)

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

    Delegations.writeNew(new FileOutputStream(in.toFile)).get

    val config = Config(SignDelegation, keyNames = List(keyName01, keyName02), inputPath = in.some, outputPath = out.some)

    handler(config).futureValue

    val signedPayload = jawn.parseFile(out.toFile).flatMap(_.as[SignedPayload[TargetsRole]]).valueOr(throw _)

    forAll(List(keyName01, keyName02)) { keyName =>
      val (pub, _) = userKeyStorage.readKeyPair(keyName).get
      val keyMap = Map(pub.id -> pub)

      TufCrypto.payloadSignatureIsValid(keyMap, threshold = 1, signedPayload = signedPayload) shouldBe a[Valid[_]]
    }
  }

  test("pushes a delegation to the server") {
    val in = Files.createTempFile("in", ".json")
    val out = Files.createTempFile("out", ".json")

    val empty = Delegations.empty

    val delegation = SignedPayload(Seq.empty, empty, empty.asJson)

    Files.write(in, delegation.asJson.spaces2.getBytes)

    val name = "delegation01".unsafeApply[DelegatedRoleName]

    val config = Config(PushDelegation, delegationName = name, inputPath = in.some, outputPath = out.some)

    handler(config).futureValue

    client.delegations().toMap.apply(name).asJsonSignedPayload shouldBe delegation.asJsonSignedPayload
  }

  test("adds a delegation to an existing targets role") {
    val key = userKeyStorage.genKeys(KeyName("mykey02"), KeyType.default).get
    val pubkey = KeyType.default.crypto.generateKeyPair().pubkey
    val keyFile = Files.createTempFile("key01.pub", ".json")
    Files.write(keyFile, pubkey.asJson.spaces2.getBytes)

    val name = "delegation02".unsafeApply[DelegatedRoleName]
    val delegatedPath = "path01/*".unsafeApply[DelegatedPathPattern]
    val config = Config(AddTargetDelegation, delegationName = name, delegatedPaths = List(delegatedPath), keyPaths = List(keyFile))

    handler(config).futureValue

    val role = tufRepo.readUnsignedRole[TargetsRole].get

    role.delegations shouldNot be(empty)
    role.delegations.get.keys shouldBe Map(pubkey.id -> pubkey)

    role.delegations.get.roles shouldNot be(empty)
    val newDelegation = role.delegations.get.roles.head

    newDelegation shouldBe ClientDataType.Delegation(name, List(pubkey.id), List(delegatedPath))
  }

  test("pulls a delegation by name") {
    val out = Files.createTempFile("out", ".json")

    val name = "delegation03".unsafeApply[DelegatedRoleName]
    val delegation = TargetsRole(Instant.now, Map.empty, version = 1)
    val signedDelegation = SignedPayload(Seq.empty, delegation, delegation.asJson)

    client.pushDelegation(name, signedDelegation).futureValue

    val config = Config(PullDelegation, delegationName = name, outputPath = out.some)

    handler(config).futureValue

    jawn.parseFile(out.toFile).flatMap(_.as[SignedPayload[TargetsRole]]).valueOr(throw _).asJsonSignedPayload shouldBe signedDelegation.asJsonSignedPayload
  }
}
