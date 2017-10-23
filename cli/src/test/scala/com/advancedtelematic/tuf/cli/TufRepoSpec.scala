package com.advancedtelematic.tuf.cli

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.time.Instant

import cats.syntax.either._
import com.advancedtelematic.libtuf.crypt.SignedPayloadSignatureOps._
import com.advancedtelematic.libtuf.data.ClientDataType.{ETag, RootRole, TargetCustom, TargetsRole}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, EdTufPrivateKey, RoleType, SignedPayload, TargetName, TargetVersion, TufKey}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.tuf.cli.CliCodecs.authConfigDecoder
import com.advancedtelematic.tuf.cli.DataType.{AuthConfig, KeyName, RepoName}
import io.circe.jawn._
import eu.timepit.refined.api.Refined

import scala.concurrent.Future
import scala.util.Try

class TufRepoSpec extends CliSpec {

  import scala.concurrent.ExecutionContext.Implicits.global

  lazy val treehubCredentials: Path = Paths.get(this.getClass.getResource("/treehub.json").toURI)
  lazy val credentialsZip: Path = Paths.get(this.getClass.getResource("/credentials.zip").toURI)

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

  test("can read auth config for an initialized repo") {
    val repo = initRepo()

    repo.init(treehubCredentials, KeyName("targets-for-init"))

    repo.authConfig().get shouldBe a[AuthConfig]
  }

  test("can initialize repo from ZIP file") {
    val repo = initRepo()

    repo.init(credentialsZip, KeyName("default-key"))

    repo.authConfig().get shouldBe a[AuthConfig]
  }

  test("can export zip file") {
    val repo = initRepo()
    repo.initFromZip(credentialsZip, KeyName("default-key"))
    // overwrite with different auth values:
    repo.init(treehubCredentials, KeyName("default-key"))
    repo.genKeys(KeyName("default-key"), EdKeyType, 256)

    RandomNames.apply()
    val tempFilename = s"/tmp/tuf-repo-spec-export-${RandomNames()}.zip"
    val tempPath = Paths.get(tempFilename)
    repo.export(KeyName("default-key"), tempPath) shouldBe Try(())

    // test the exported zip file by creating another repo from it:
    val repoFromExported = initRepo()
    repoFromExported.initFromZip(tempPath, KeyName("default-key"))
    val credJson = parseFile(repoFromExported.repoPath.resolve("auth.json").toFile)
    val oauth2Val = credJson.right.get.as[AuthConfig](authConfigDecoder)
    oauth2Val.right.get.client_id shouldBe "fake-client-id"
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

  test("new root role contains new root id") {
    val repo = initRepo()

    val (pub, pubT, signedPayload) = rotate(repo).futureValue

    val rootRole = signedPayload.signed

    rootRole.roles(RoleType.ROOT).keyids should contain(pub.id)
    rootRole.roles(RoleType.TARGETS).keyids should contain(pubT.id)
  }

  test("new root role has proper version bump") {
    val repo = initRepo()

    val (pub, pubT, signedPayload) = rotate(repo).futureValue

    val rootRole = signedPayload.signed

    rootRole.version shouldBe 2
  }

  test("rotate key is signed by both root keys") {
    val repo = initRepo()

    val (newPubKey, _, signedPayload) = rotate(repo).futureValue
    val oldPubKey = repo.storage.readPublicKey(KeyName(s"oldroot${repo.name.value}")).get

    signedPayload.isValidFor(newPubKey) shouldBe true
    signedPayload.isValidFor(oldPubKey) shouldBe true
  }

  test("saves deleted root when rotating") {
    val repo = initRepo()

    rotate(repo).futureValue

    val oldPrivateKey = repo.storage.readPrivateKey(KeyName(s"oldroot${repo.name.value}")).get

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

    val path = repo.addTarget(TargetName("fake-one"), TargetVersion("1.2.3"), 100, Refined.unsafeApply("03aa3f5e2779b625a455651b54866447f995a2970d164581b4073044435359ed"), List.empty, new URI("https://ats.com")).get
    val role = parseFile(path.toFile).flatMap(_.as[TargetsRole]).valueOr(throw _)

    role.targets.keys.map(_.value) should contain("fake-one-1.2.3")
    role.targets.values.head.customParsed[TargetCustom].flatMap(_.uri) should contain(new URI("https://ats.com"))
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
    import com.advancedtelematic.libtuf.data.ClientDataType.RoleTypeToMetaPathOp
    import scala.collection.JavaConverters._

    val repo = initRepo()

    val reposerverClient = new FakeUserReposerverClient

    val (_, pubTargets, _) = rotate(repo, reposerverClient).futureValue

    repo.signTargets(KeyName(s"targets${repo.name.value}")).get
    Files.write(repo.repoPath.resolve("roles").resolve(RoleType.TARGETS.toETagPath), Seq("etag").asJava)

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

  test("save targets") {
    val repo = initRepo()

    val rootName = KeyName("root")
    repo.genKeys(rootName, EdKeyType, 256).get

    val path = repo.signTargets(rootName).get
    val signedTargetRole = parseFile(path.toFile).flatMap(_.as[SignedPayload[TargetsRole]]).valueOr(throw _)

    repo.saveTargets(signedTargetRole, ETag("etag")) shouldBe 'success
  }
}
