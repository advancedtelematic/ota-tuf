package com.advancedtelematic.tuf.cli

import java.net.URI
import java.nio.file.Files
import java.time.Instant
import java.time.temporal.ChronoUnit

import cats.syntax.either._
import com.advancedtelematic.libtuf.crypt.SignedPayloadSignatureOps._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.TufRole._
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole, TargetCustom, TargetsRole, TufRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, KeyType, RoleType, RsaKeyType, SignedPayload, TargetFormat, TargetName, TargetVersion, ValidTargetFilename}
import com.advancedtelematic.libtuf.reposerver.UserTufServerClient
import com.advancedtelematic.tuf.cli.DataType.{KeyName, RepoName}
import com.advancedtelematic.tuf.cli.repo.TufRepo.{RoleMissing, RootPullError}
import com.advancedtelematic.tuf.cli.repo.{CliKeyStorage, DirectorRepo, RepoServerRepo, TufRepo}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.refineV
import io.circe.jawn._
import io.circe.syntax._
import org.scalactic.source.Position

import scala.util.Success

class TufRepoSpec extends CliSpec with KeyTypeSpecSupport {

  import scala.concurrent.ExecutionContext.Implicits.global

  def initRepoServerRepo(keyType: KeyType): RepoServerRepo = {
    val repo = new RepoServerRepo(RepoName(RandomNames() + "-reposerver-repo"), Files.createTempDirectory("tuf-repo").resolve("repo"))
    repo.initRepoDirs().get
    repo.initTargets(11, Instant.now).get

    val rootKeys = repo.genKeys(KeyName("root"), keyType).get
    val targetKeys = repo.genKeys(KeyName("target"), keyType).get

    val keys = Map(rootKeys.pubkey.id -> rootKeys.pubkey, targetKeys.pubkey.id -> targetKeys.pubkey)

    val roles = Map(
      RoleType.ROOT -> RoleKeys(Seq(rootKeys.pubkey.id), threshold = 1),
      RoleType.TARGETS -> RoleKeys(Seq(targetKeys.pubkey.id), threshold = 1)
    )

    val rootRole = RootRole(keys, roles, version = 1, Instant.now.plus(365, ChronoUnit.DAYS))

    repo.writeUnsignedRole(rootRole)

    repo
  }

  def initDirectorRepo(keyType: KeyType): DirectorRepo = {
    val repo = new DirectorRepo(RepoName(RandomNames() + "-director-repo"), Files.createTempDirectory("tuf-repo").resolve("repo"))
    repo.initRepoDirs().get

    val rootKeys = repo.genKeys(KeyName("root"), keyType).get
    val targetKeys = repo.genKeys(KeyName("target"), keyType).get

    val keys = Map(rootKeys.pubkey.id -> rootKeys.pubkey, targetKeys.pubkey.id -> targetKeys.pubkey)

    val roles = Map(
      RoleType.ROOT -> RoleKeys(Seq(rootKeys.pubkey.id), threshold = 1),
      RoleType.TARGETS -> RoleKeys(Seq(targetKeys.pubkey.id), threshold = 1)
    )

    val rootRole = RootRole(keys, roles, version = 1, Instant.now.plus(365, ChronoUnit.DAYS))

    repo.writeUnsignedRole(rootRole)

    repo
  }

  def keyTypeRepoTest(name: String)(fn: (KeyType, TufRepo) => Any)(implicit pos: Position): Unit = {
    keyTypeTest(name + " Reposerver") { keyType => fn(keyType, initRepoServerRepo(keyType)) }
    keyTypeTest(name + " Director") { keyType => fn(keyType, initDirectorRepo(keyType)) }
  }

  def clientTest(name: String)(fn: (KeyType, TufRepo, FakeUserClient) => Any)(implicit pos: Position): Unit = {
    keyTypeTest(name + " Reposerver") { keyType => fn(keyType, initRepoServerRepo(keyType), FakeUserReposerverClient(keyType)) }
    keyTypeTest(name + " Director") { keyType => fn(keyType, initDirectorRepo(keyType), FakeUserDirectorClient(keyType)) }
  }

  keyTypeRepoTest("adds a key to a repo ") { (keyType, repo) =>
    repo.genKeys(KeyName("newkey"), keyType)

    Files.exists(repo.repoPath.resolve("keys").resolve("newkey.pub")) shouldBe true
  }

  keyTypeTest("does not overwrite existing unsigned targets.json during rotate") { keyType =>
    val repo = initRepoServerRepo(keyType)

    val client = FakeUserReposerverClient(keyType)

    val signedTargets = repo.readUnsignedRole[TargetsRole].get

    client.moveOffline(repo, keyType).futureValue

    repo.readUnsignedRole[TargetsRole].get.asJson shouldBe signedTargets.asJson
  }

  keyTypeTest("pulls targets.json from reposerver during rotate") { keyType =>
    val repo = initRepoServerRepo(keyType)

    val client = FakeUserReposerverClient(keyType)

    Files.delete(repo.repoPath.resolve("roles/unsigned/targets.json"))

    val signedTargets = client.targets().futureValue

    client.moveOffline(repo, keyType).futureValue

    repo.readUnsignedRole[TargetsRole].get.asJson shouldBe signedTargets.targets.signed.asJson

    repo.repoPath.resolve("roles/targets.json.checksum").toFile.exists() shouldBe true
  }

  keyTypeTest("new root role does not contain old targets keys") { keyType =>
    val repo = initRepoServerRepo(keyType)
    val client = FakeUserReposerverClient(keyType)
    val oldTargetsKeyId = client.root().map(_.signed.roles(RoleType.TARGETS).keyids.head).futureValue
    val (_, pubT, signedPayload) = client.moveOffline(repo, keyType).futureValue
    val rootRole = signedPayload.signed

    rootRole.keys.keys should contain(pubT.id)
    rootRole.keys.keys shouldNot contain(oldTargetsKeyId)
  }

  keyTypeTest("root after rotate contains new key ids") { keyType =>
    val repo = initRepoServerRepo(keyType)
    val (pub, pubT, signedPayload) = FakeUserReposerverClient(keyType).moveOffline(repo, keyType).futureValue

    signedPayload.signed shouldBe a[RootRole]
    signedPayload.signed.keys.keys should contain(pub.id)
    signedPayload.signed.keys.values should contain(pub)
    signedPayload.signed.keys.keys should contain(pubT.id)
    signedPayload.signed.keys.values should contain(pubT)
  }

  keyTypeTest("root after rotate is properly signed") { keyType =>
    val repo = initDirectorRepo(keyType)

    val client = FakeUserDirectorClient(keyType)

    val oldRoot = client.root().futureValue.signed
    val oldRootPubKeyId = oldRoot.roles(RoleType.ROOT).keyids.head
    val oldRootPub = oldRoot.keys(oldRootPubKeyId)

    val (pub, pubT, signedPayload) = client.moveOffline(repo, keyType).futureValue

    signedPayload.isValidFor(pub)
    signedPayload.isValidFor(oldRootPub)
  }

  keyTypeTest("new root role contains new root id") { keyType =>
    val repo = initDirectorRepo(keyType)
    val client = FakeUserDirectorClient(keyType)
    val (pub, pubT, signedPayload) = client.moveOffline(repo, keyType).futureValue
    val rootRole = signedPayload.signed

    rootRole.roles(RoleType.ROOT).keyids should contain(pub.id)
    rootRole.roles(RoleType.TARGETS).keyids should contain(pubT.id)

    rootRole.keys.keys should contain(pub.id)
    rootRole.keys.keys should contain(pubT.id)
  }

  keyTypeTest("new root role has proper version bump") { keyType =>
    val repo = initDirectorRepo(keyType)
    val client = FakeUserDirectorClient(keyType)

    val (pub, pubT, signedPayload) = client.moveOffline(repo, keyType).futureValue

    val rootRole = signedPayload.signed

    rootRole.version shouldBe 2
  }

  keyTypeTest("rotate key is signed by both root keys") { keyType =>
    val repo = initDirectorRepo(keyType)
    val client = FakeUserDirectorClient(keyType)

    val keyStorage = new CliKeyStorage(repo.repoPath)
    val (newPubKey, _, signedPayload) = client.moveOffline(repo, keyType).futureValue
    val oldPubKey = keyStorage.readPublicKey(KeyName(s"oldroot${repo.name.value}")).get

    signedPayload.isValidFor(newPubKey) shouldBe true
    signedPayload.isValidFor(oldPubKey) shouldBe true
  }

  keyTypeTest("saves deleted root when rotating") { keyType =>
    val repo = initDirectorRepo(keyType)
    val client = FakeUserDirectorClient(keyType)

    val keyStorage = new CliKeyStorage(repo.repoPath)
    client.moveOffline(repo, keyType).futureValue
    val oldPrivateKey = keyStorage.readPrivateKey(KeyName(s"oldroot${repo.name.value}")).get

    oldPrivateKey.keytype shouldBe keyType
  }

  keyTypeTest("pushed targets are validated against new targets key when moving root offline") { keyType =>
    import scala.collection.JavaConverters._

    val repo = initRepoServerRepo(keyType)
    val client = FakeUserReposerverClient(keyType)
    val (_, pubTargets, _) = client.moveOffline(repo, keyType).futureValue
    repo.signTargets(Seq(KeyName(s"targets${repo.name.value}"))).get
    Files.write(repo.repoPath.resolve("roles").resolve(TufRole.targetsTufRole.checksumPath), Seq("997890bc85c5796408ceb20b0ca75dabe6fe868136e926d24ad0f36aa424f99d").asJava)

    val payload = repo.pushTargets(client).futureValue

    payload.isValidFor(pubTargets) shouldBe true
  }

  keyTypeTest("initTargets creates an empty target") { keyType =>
    val now = Instant.now

    val repo = initRepoServerRepo(keyType)

    val path = repo.initTargets(20, now.plusSeconds(1)).get
    val role = parseFile(path.toFile).flatMap(_.as[TargetsRole]).valueOr(throw _)

    role.targets should be(empty)

    role.expires.isAfter(now) shouldBe true
    role.version shouldBe 20
  }

  keyTypeTest("fails if target does not exist") { keyType =>
    val repo = initRepoServerRepo(keyType)

    val targetFilename = refineV[ValidTargetFilename]("fake-one-1.2.3").right.get

    val path = repo.deleteTarget(targetFilename)

    path.failed.get shouldBe a[IllegalArgumentException]
  }

  keyTypeTest("deletes an existing target targets") { keyType =>
    val repo = initRepoServerRepo(keyType)

    val targetFilename = refineV[ValidTargetFilename]("fake-one-1.2.3").right.get

    repo.addTarget(TargetName("fake-one"), TargetVersion("1.2.3"), 100, Refined.unsafeApply("03aa3f5e2779b625a455651b54866447f995a2970d164581b4073044435359ed"), List.empty, Some(URI.create("https://ats.com")), TargetFormat.BINARY).get

    val path = repo.deleteTarget(targetFilename).get

    val role = parseFile(path.toFile).flatMap(_.as[TargetsRole]).valueOr(throw _)

    role.targets.keys shouldNot contain(targetFilename)
  }

  keyTypeTest("adds a target to an existing targets") { keyType =>
    val repo = initRepoServerRepo(keyType)

    val path = repo.addTarget(TargetName("fake-one"), TargetVersion("1.2.3"), 100, Refined.unsafeApply("03aa3f5e2779b625a455651b54866447f995a2970d164581b4073044435359ed"), List.empty, Some(URI.create("https://ats.com")), TargetFormat.BINARY).get
    val role = parseFile(path.toFile).flatMap(_.as[TargetsRole]).valueOr(throw _)

    role.targets.keys.map(_.value) should contain("fake-one-1.2.3")
    role.targets.values.head.customParsed[TargetCustom].flatMap(_.uri) should contain(new URI("https://ats.com"))
  }

  keyTypeTest("adds a target to an existing targets with specified format") { keyType =>
    val repo = initRepoServerRepo(keyType)

    val path = repo.addTarget(TargetName("fake-ostree"), TargetVersion("1.2.3"), 100, Refined.unsafeApply("03aa3f5e2779b625a455651b54866447f995a2970d164581b4073044435359ed"), List.empty, Some(URI.create("https://ats.com")), TargetFormat.OSTREE).get
    val role = parseFile(path.toFile).flatMap(_.as[TargetsRole]).valueOr(throw _)

    val format = role.targets.get(Refined.unsafeApply("fake-ostree-1.2.3")).flatMap(_.customParsed[TargetCustom]).flatMap(_.targetFormat)
    format should contain(TargetFormat.OSTREE)
  }

  keyTypeTest("bumps version when signing targets role") { keyType =>
    val repo = initRepoServerRepo(keyType)
    val previousExpires = repo.readUnsignedRole[TargetsRole].get.expires

    val targetsKeyName = KeyName("somekey")
    repo.genKeys(targetsKeyName, keyType).get

    repo.signTargets(Seq(targetsKeyName)).get

    val payload = repo.readSignedRole[TargetsRole].get
    payload.signed.version shouldBe 12
    payload.signed.expires should be > previousExpires
  }

  keyTypeTest("sets version when specified ") { keyType =>
    val repo = initRepoServerRepo(keyType)

    val targetsKeyName = KeyName("somekey")
    repo.genKeys(targetsKeyName, keyType).get

    repo.signTargets(Seq(targetsKeyName), Option(21)).get

    val payload = repo.readSignedRole[TargetsRole].get
    payload.signed.version shouldBe 21
  }

  keyTypeTest("signs targets") { keyType =>
    val repo = initRepoServerRepo(keyType)
    val targetsKeyName = KeyName("somekey")
    val pub = repo.genKeys(targetsKeyName, keyType).get.pubkey

    val path = repo.signTargets(Seq(targetsKeyName)).get
    val payload = parseFile(path.toFile).flatMap(_.as[SignedPayload[TargetsRole]]).valueOr(throw _)

    payload.signatures.map(_.keyid) should contain(pub.id)

    payload.isValidFor(pub) shouldBe true
  }

  keyTypeTest("saves targets.json and checksum to file when pulling") { keyType =>
    val repo = initRepoServerRepo(keyType)

    val reposerverClient = FakeUserReposerverClient(keyType)

    val rootRole = reposerverClient.root().futureValue

    repo.pullTargets(reposerverClient, rootRole.signed).futureValue

    repo.readUnsignedRole[TargetsRole].get shouldBe a[TargetsRole]

    Files.readAllLines(repo.repoPath.resolve("roles/targets.json.checksum")).get(0) shouldNot be(empty)
  }

  clientTest("can pull a root.json when no local root is available, when forcing") { (keyType, repo, client) =>
    val newRoot = repo.pullRoot(client, skipLocalValidation = true).futureValue

    val signed = repo.readSignedRole[RootRole]
    signed shouldBe a[Success[_]]

    signed.get.asJson shouldBe newRoot.asJson

    val rootRole = repo.readUnsignedRole[RootRole]
    rootRole.get.asJson shouldBe newRoot.signed.asJson
  }

  keyTypeRepoTest("adds root key to unsigned root") { (keyType,repo) =>
    val keyname = KeyName("somekey")
    val keyPair = repo.genKeys(keyname, keyType).get

    repo.addRootKeys(List(keyname))

    val rootRole = repo.readUnsignedRole[RootRole].get

    rootRole.keys(keyPair.pubkey.id) shouldBe keyPair.pubkey
    rootRole.roles(RoleType.ROOT).keyids should contain(keyPair.pubkey.id)
  }

  keyTypeRepoTest("removes root key from unsigned root") { (keyType,repo) =>
    val keyname = KeyName("somekey")
    val keyPair = repo.genKeys(keyname, keyType).get

    repo.addRootKeys(List(keyname))
    val keyIds = repo.keyIdsByName(List(KeyName("root"))).get
    repo.removeRootKeys(keyIds)

    val rootRole = repo.readUnsignedRole[RootRole].get

    rootRole.roles(RoleType.ROOT).keyids shouldBe Seq(keyPair.pubkey.id)
  }

  keyTypeRepoTest("can remove keys using key ids") { (keyType,repo) =>
    val keyname = KeyName("somekey")
    val keyPair = repo.genKeys(keyname, keyType).get
    val othername = KeyName("otherkey")
    val otherKeyPair = repo.genKeys(othername, keyType).get

    repo.addRootKeys(List(keyname, othername))
    repo.removeRootKeys(List(keyPair.pubkey.id))

    val rootRole = repo.readUnsignedRole[RootRole].get

    rootRole.roles(RoleType.ROOT).keyids should contain(otherKeyPair.pubkey.id)
    rootRole.roles(RoleType.ROOT).keyids shouldNot contain(keyPair.pubkey.id)
  }

  clientTest("pull succeeds when new root.json is valid against local root.json") { (keyType, repo, client) =>
    val oldRoot = repo.pullRoot(client, skipLocalValidation = true).futureValue

    val newUnsignedRoot = oldRoot.signed.copy(version = oldRoot.signed.version + 1)
    val newRoot = client.sign(newUnsignedRoot)

    client.pushSignedRoot(newRoot).futureValue

    repo.pullRoot(client, skipLocalValidation = false).futureValue
  }

  clientTest("pull fails when new root.json is not the same as old root but has same version numbers") { (keyType, repo, client) =>
    val oldSignedRoot = repo.pullRoot(client, skipLocalValidation = true).futureValue

    val newRoot = oldSignedRoot.signed.copy(expires = Instant.now().plus(100, ChronoUnit.DAYS))
    client.setRoot(client.sign(newRoot))

    val error = repo.pullRoot(client, skipLocalValidation = false).failed.futureValue

    error shouldBe a[RootPullError]
    error.asInstanceOf[RootPullError].errors.head shouldBe "New root has same version as old root but is not the same root.json"
  }

  clientTest("pull succeeds when new root.json is the same as old json") { (keyType, repo, client) =>
    repo.pullRoot(client, skipLocalValidation = true).futureValue

    repo.pullRoot(client, skipLocalValidation = false).futureValue shouldBe a[SignedPayload[_]]
  }

  clientTest("pull fails when new root.json is not valid against local root.json") { (keyType, repo, client) =>
    val oldRoot = repo.pullRoot(client, skipLocalValidation = true).futureValue

    val newUnsignedRoot = oldRoot.signed.copy(version = oldRoot.signed.version + 1)
    client.setRoot(SignedPayload(Seq.empty, newUnsignedRoot, newUnsignedRoot.asJson))

    val error = repo.pullRoot(client, skipLocalValidation = false).failed.futureValue

    val oldKeyId = oldRoot.signed.roles(RoleType.ROOT).keyids.head

    error shouldBe a[RootPullError]
    error.getMessage should include(s"No signature found for key $oldKeyId")
    error.getMessage should include(s"Root role version 1 requires 1 valid signatures in version 2, 0 supplied")
  }

  clientTest("fails with proper error when cannot find root at specified version") { (keyType, repo, client) =>
    val oldRoot = repo.pullRoot(client, skipLocalValidation = true).futureValue

    val newUnsignedRoot = oldRoot.signed.copy(version = oldRoot.signed.version + 10)
    client.setRoot(SignedPayload(Seq.empty, newUnsignedRoot, newUnsignedRoot.asJson))

    val error = repo.pullRoot(client, skipLocalValidation = false).failed.futureValue

    error shouldBe a[RootPullError]
    error.getMessage should include(s"role with version 2 not found")
  }

  clientTest("validates a root chain") { (keyType, repo, client) =>
    val oldRoot = repo.pullRoot(client, skipLocalValidation = true).futureValue

    for(i <- 1 until 10) {
      val newUnsignedRoot = oldRoot.signed.copy(version = oldRoot.signed.version + i)
      val newRoot = client.sign(newUnsignedRoot)
      client.pushSignedRoot(newRoot).futureValue
    }

    val newRoot = repo.pullRoot(client, skipLocalValidation = false).futureValue

    newRoot shouldBe a[SignedPayload[_]]
    newRoot.signed shouldBe a[RootRole]
  }

  clientTest("pull fails when local root does not exist") { (keyType, repo, client) =>
    val error = repo.pullRoot(client, skipLocalValidation = false).failed.futureValue

    error shouldBe a[RoleMissing[_]]
  }

  clientTest("can push root.json") { (keyType, repo, client) =>
    repo.pullRoot(client, skipLocalValidation = true).futureValue

    repo.pushRoot(client).futureValue

    val signed = repo.readSignedRole[RootRole]
    signed shouldBe a[Success[_]]
  }

  keyTypeRepoTest("signs root") { (keyType, repo) =>
    val keyname = KeyName("somekey")
    val pub = repo.genKeys(keyname, keyType).get.pubkey

    val keyname02 = KeyName("somekey02")
    val pub02 = repo.genKeys(keyname02, keyType).get.pubkey

    val path = repo.signRoot(Seq(keyname, keyname02)).get
    val payload = parseFile(path.toFile).flatMap(_.as[SignedPayload[RootRole]]).valueOr(throw _)

    payload.isValidFor(pub) shouldBe true
    payload.isValidFor(pub02) shouldBe true
  }

  keyTypeRepoTest("signing root increases version") { (keyType, repo) =>
    val keyname = KeyName("somekey")
    val pub = repo.genKeys(keyname, keyType).get.pubkey

    val path = repo.signRoot(Seq(keyname)).get
    val payload = parseFile(path.toFile).flatMap(_.as[SignedPayload[RootRole]]).valueOr(throw _)

    payload.signed.version shouldBe 2
  }
}
