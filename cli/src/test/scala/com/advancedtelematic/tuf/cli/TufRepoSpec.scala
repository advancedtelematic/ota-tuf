package com.advancedtelematic.tuf.cli

import java.net.URI
import java.nio.file.Files
import java.time.Instant

import cats.syntax.either._
import com.advancedtelematic.libtuf.crypt.SignedPayloadSignatureOps._
import com.advancedtelematic.libtuf.data.ClientDataType.{TufRole, RootRole, TargetCustom, TargetsRole}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, EdTufPrivateKey, RoleType, SignedPayload, TargetFormat, TargetName, TargetVersion, TufKey}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.tuf.cli.DataType.{KeyName, RepoName}
import com.advancedtelematic.tuf.cli.repo.{CliKeyStorage, TufRepo}
import io.circe.jawn._
import eu.timepit.refined.api.Refined
import com.advancedtelematic.libtuf.data.ClientDataType.TufRole._

import scala.concurrent.Future
import io.circe.syntax._

class TufRepoSpec extends CliSpec {

  import scala.concurrent.ExecutionContext.Implicits.global

  def initRepo(): TufRepo = {
    val repo = new TufRepo(RepoName(RandomNames() + "-repo"), Files.createTempDirectory("tuf-repo"))
    repo.initTargets(11, Instant.now).get
    repo
  }

  def rotate(repo: TufRepo, reposerverClient: FakeUserReposerverClient = new FakeUserReposerverClient): Future[(TufKey, TufKey, SignedPayload[RootRole])] = {
    val oldRootName = KeyName(s"oldroot${repo.name.value}")
    val newRootName = KeyName(s"newroot${repo.name.value}")
    val newTargetsName = KeyName(s"targets${repo.name.value}")

    val (pub, _) = repo.genKeys(newRootName, EdKeyType, 256).get
    val (pubT, _) = repo.genKeys(newTargetsName, EdKeyType, 256).get

    repo.rotateRoot(reposerverClient, newRootName, oldRootName, newTargetsName, None).map { s =>
      (pub, pubT, s)
    }
  }

  test("adds a key to a repo") {
    val repo = initRepo()

    repo.genKeys(KeyName("newkey"), EdKeyType, 256)

    Files.exists(repo.repoPath.resolve("keys").resolve("newkey.pub")) shouldBe true
  }

  test("root after rotate contains new key ids") {
    val repo = initRepo()

    val (pub, pubT, signedPayload) = rotate(repo).futureValue

    signedPayload.signed shouldBe a[RootRole]
    signedPayload.signed.keys.keys should contain(pub.id)
    signedPayload.signed.keys.values should contain(pub)
    signedPayload.signed.keys.keys should contain(pubT.id)
    signedPayload.signed.keys.values should contain(pubT)
  }

  test("root after rotate is properly signed") {
    val repo = initRepo()

    val client = new FakeUserReposerverClient()

    val oldRoot = client.root().futureValue.signed
    val oldRootPubKeyId = oldRoot.roles(RoleType.ROOT).keyids.head
    val oldRootPub = oldRoot.keys(oldRootPubKeyId)

    val (pub, pubT, signedPayload) = rotate(repo, client).futureValue

    signedPayload.isValidFor(pub)
    signedPayload.isValidFor(oldRootPub)
  }

  test("does not overwrite existing unsigned targets.json during rotate") {
    val repo = initRepo()

    val client = new FakeUserReposerverClient()

    val signedTargets = repo.readUnsignedRole[TargetsRole].get

    rotate(repo, client).futureValue

    repo.readUnsignedRole[TargetsRole].get.asJson shouldBe signedTargets.asJson
  }

  test("pulls targets.json from reposerver during rotate") {
    val repo = initRepo()

    val client = new FakeUserReposerverClient()

    Files.delete(repo.repoPath.resolve("roles/unsigned/targets.json"))

    val signedTargets = client.targets().futureValue

    rotate(repo, client).futureValue

    repo.readUnsignedRole[TargetsRole].get.asJson shouldBe signedTargets.targets.signed.asJson

    repo.repoPath.resolve("roles/targets.json.etag").toFile.exists() shouldBe true
  }

  test("new root role contains new root id") {
    val repo = initRepo()

    val (pub, pubT, signedPayload) = rotate(repo).futureValue

    val rootRole = signedPayload.signed

    rootRole.roles(RoleType.ROOT).keyids should contain(pub.id)
    rootRole.roles(RoleType.TARGETS).keyids should contain(pubT.id)

    rootRole.keys.keys should contain(pub.id)
     rootRole.keys.keys should contain(pubT.id)
  }

  test("new root role does not contain old targets keys") {
    val repo = initRepo()

    val reposerverClient = new FakeUserReposerverClient

    val oldTargetsKeyId = reposerverClient.root().map(_.signed.roles(RoleType.TARGETS).keyids.head).futureValue
    val (_, pubT, signedPayload) = rotate(repo, reposerverClient).futureValue

    val rootRole = signedPayload.signed

    rootRole.keys.keys should contain(pubT.id)
    rootRole.keys.keys shouldNot contain(oldTargetsKeyId)
  }

  test("new root role has proper version bump") {
    val repo = initRepo()

    val (pub, pubT, signedPayload) = rotate(repo).futureValue

    val rootRole = signedPayload.signed

    rootRole.version shouldBe 2
  }

  test("rotate key is signed by both root keys") {
    val repo = initRepo()
    val keyStorage = new CliKeyStorage(repo.repoPath)

    val (newPubKey, _, signedPayload) = rotate(repo).futureValue
    val oldPubKey = keyStorage.readPublicKey(KeyName(s"oldroot${repo.name.value}")).get

    signedPayload.isValidFor(newPubKey) shouldBe true
    signedPayload.isValidFor(oldPubKey) shouldBe true
  }

  test("saves deleted root when rotating") {
    val repo = initRepo()
    val keyStorage = new CliKeyStorage(repo.repoPath)

    rotate(repo).futureValue

    val oldPrivateKey = keyStorage.readPrivateKey(KeyName(s"oldroot${repo.name.value}")).get

    oldPrivateKey shouldBe a[EdTufPrivateKey]
  }

  test("initTargets creates an empty target") {
    val now = Instant.now

    val repo = new TufRepo(RepoName(RandomNames() + "-repo"), Files.createTempDirectory("tuf-repo"))

    val path = repo.initTargets(20, now.plusSeconds(1)).get
    val role = parseFile(path.toFile).flatMap(_.as[TargetsRole]).valueOr(throw _)

    role.targets should be(empty)

    role.expires.isAfter(now) shouldBe true
    role.version shouldBe 20
  }

  test("adds a target to an existing targets") {
    val repo = initRepo()

    val path = repo.addTarget(TargetName("fake-one"), TargetVersion("1.2.3"), 100, Refined.unsafeApply("03aa3f5e2779b625a455651b54866447f995a2970d164581b4073044435359ed"), List.empty, new URI("https://ats.com"), TargetFormat.BINARY).get
    val role = parseFile(path.toFile).flatMap(_.as[TargetsRole]).valueOr(throw _)

    role.targets.keys.map(_.value) should contain("fake-one-1.2.3")
    role.targets.values.head.customParsed[TargetCustom].flatMap(_.uri) should contain(new URI("https://ats.com"))
  }

  test("adds a target to an existing targets with specified format") {
    val repo = initRepo()

    val path = repo.addTarget(TargetName("fake-ostree"), TargetVersion("1.2.3"), 100, Refined.unsafeApply("03aa3f5e2779b625a455651b54866447f995a2970d164581b4073044435359ed"), List.empty, new URI("https://ats.com"), TargetFormat.OSTREE).get
    val role = parseFile(path.toFile).flatMap(_.as[TargetsRole]).valueOr(throw _)

    val format = role.targets.get(Refined.unsafeApply("fake-ostree-1.2.3")).flatMap(_.customParsed[TargetCustom]).flatMap(_.targetFormat)
    format should contain(TargetFormat.OSTREE)
  }

  test("bumps version when signing targets role") {
    val repo = initRepo()

    val targetsKeyName = KeyName("somekey")
    repo.genKeys(targetsKeyName, EdKeyType, 256).get

    repo.signTargets(targetsKeyName).get

    val payload = repo.readSignedRole[TargetsRole].get
    payload.signed.version shouldBe 12

    val unsignedPayload = repo.readUnsignedRole[TargetsRole].get
    unsignedPayload.version shouldBe 12
  }

  test("signs targets") {
    val repo = initRepo()

    val targetsKeyName = KeyName("somekey")
    val (pub, _) = repo.genKeys(targetsKeyName, EdKeyType, 256).get

    val path = repo.signTargets(targetsKeyName).get
    val payload = parseFile(path.toFile).flatMap(_.as[SignedPayload[TargetsRole]]).valueOr(throw _)

    payload.signatures.map(_.keyid) should contain(pub.id)

    payload.isValidFor(pub) shouldBe true
  }

  test("pushes targets to reposerver") {
    import scala.collection.JavaConverters._

    val repo = initRepo()

    val reposerverClient = new FakeUserReposerverClient

    val (_, pubTargets, _) = rotate(repo, reposerverClient).futureValue

    repo.signTargets(KeyName(s"targets${repo.name.value}")).get
    Files.write(repo.repoPath.resolve("roles").resolve(TufRole.targetsTufRole.toETagPath), Seq("etag").asJava)

    val payload = repo.pushTargets(reposerverClient).futureValue

    payload.isValidFor(pubTargets) shouldBe true
  }

  test("pushes targets pub key to reposerver") {
    val repo = initRepo()

    val reposerverClient = new FakeUserReposerverClient

    val (_, pubTargets, _) = rotate(repo, reposerverClient).futureValue

    val pushedKey = repo.pushTargetsKey(reposerverClient, KeyName(s"targets${repo.name.value}")).futureValue

    pushedKey shouldBe pubTargets
  }


  test("saves targets.json and etags to file when pulling") {
    val repo = initRepo()

    val reposerverClient = new FakeUserReposerverClient

    val rootRole = reposerverClient.root().futureValue

    repo.pullTargets(reposerverClient, rootRole.signed).futureValue

    repo.readUnsignedRole[TargetsRole].get shouldBe a[TargetsRole]

    Files.readAllLines(repo.repoPath.resolve("roles/targets.json.etag")).get(0) shouldNot be(empty)
  }
}
