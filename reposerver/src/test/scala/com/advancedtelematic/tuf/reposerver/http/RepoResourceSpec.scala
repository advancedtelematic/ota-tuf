package com.advancedtelematic.tuf.reposerver.http

import java.time.Instant
import java.time.temporal.ChronoUnit

import eu.timepit.refined.refineV
import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.scaladsl.Sink
import akka.stream.testkit.scaladsl.TestSink
import akka.util.ByteString
import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.show._
import com.advancedtelematic.libats.codecs.CirceCodecs._
import com.advancedtelematic.libats.data.DataType.HashMethod
import com.advancedtelematic.libats.data.ErrorRepresentation
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libats.http.HttpCodecs._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.RoleTypeOps
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientHashes, ClientTargetItem, RootRole, SnapshotRole, TargetCustom, TargetsRole, TimestampRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, _}
import com.advancedtelematic.libtuf_server.crypto.Sha256Digest
import com.advancedtelematic.libtuf_server.data.Messages.{PackageStorageUsage, TufTargetAdded}
import com.advancedtelematic.libtuf_server.data.Requests._
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient.RequestTargetItem
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.SignedRole
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepositorySupport
import com.advancedtelematic.tuf.reposerver.db.SignedRoleDbTestUtil._
import com.advancedtelematic.tuf.reposerver.target_store.TargetStoreEngine.{TargetBytes, TargetRetrieveResult}
import com.advancedtelematic.tuf.reposerver.util.NamespaceSpecOps._
import com.advancedtelematic.tuf.reposerver.util._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import eu.timepit.refined.api.Refined
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import org.scalatest.prop.Whenever
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{Assertion, BeforeAndAfterAll, Inspectors, Suite}

import scala.concurrent.Future


trait RepoSupport extends ResourceSpec with SignedRoleRepositorySupport with ScalaFutures with ScalatestRouteTest { this: Suite ⇒
  implicit val ec = executor

  def makeRoleChecksumHeader(repoId: RepoId) =
    RoleChecksumHeader(signedRoleRepository.find[TargetsRole](repoId).futureValue.checksum.hash)

  val testFile = {
    val checksum = Sha256Digest.digest("hi".getBytes)
    RequestTargetItem(Uri("https://ats.com/testfile"), checksum, targetFormat = None, name = None, version = None, hardwareIds = Seq.empty, length = "hi".getBytes.length)
  }

  def addTargetToRepo(repoId: RepoId = RepoId.generate(), keyType: KeyType = RsaKeyType): RepoId = {
    fakeKeyserverClient.createRoot(repoId, keyType).futureValue

    Post(apiUri(s"repo/${repoId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      repoId
    }
  }

  def buildSignedTargetsRole(repoId: RepoId, targets: Map[TargetFilename, ClientTargetItem], version: Int = 2): SignedPayload[TargetsRole] = {
    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), targets, version)
    val signedPayload = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, targetsRole.asJson).futureValue
    SignedPayload(signedPayload.signatures, targetsRole, targetsRole.asJson)
  }

  def createOfflineTargets(filename: TargetFilename = offlineTargetFilename) = {
    val targetCustomJson =
      TargetCustom(TargetName("name"), TargetVersion("version"), Seq.empty, TargetFormat.BINARY.some)
        .asJson
        .deepMerge(Json.obj("uri" -> Uri("https://ats.com").asJson))

    val hashes: ClientHashes = Map(HashMethod.SHA256 -> Refined.unsafeApply("8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"))

    Map(filename -> ClientTargetItem(hashes, 0, targetCustomJson.some))
  }

  val offlineTargetFilename: TargetFilename = Refined.unsafeApply("some/file/name")

  val offlineTargets = createOfflineTargets(offlineTargetFilename)
}

class RepoResourceSpec extends TufReposerverSpec with RepoSupport
  with ResourceSpec with BeforeAndAfterAll with Inspectors with Whenever with PatienceConfiguration with SignedRoleRepositorySupport {

  val repoId = RepoId.generate()

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  override def beforeAll(): Unit = {
    super.beforeAll()
    fakeKeyserverClient.createRoot(repoId).futureValue
  }

  test("POST returns latest signed json") {
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[JsonSignedPayload]
      signaturesShouldBeValid(repoId, RoleType.TARGETS, signedPayload)

      val signed = signedPayload.signed
      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile".refineTry[ValidTargetFilename].get).length shouldBe 2
    }
  }

  test("POST returns json with previous elements") {
    Post(apiUri(s"repo/${repoId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Post(apiUri(s"repo/${repoId.show}/targets/myfile02"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[JsonSignedPayload].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile01".refineTry[ValidTargetFilename].get).length shouldBe 2
      targetsRole.targets("myfile02".refineTry[ValidTargetFilename].get).length shouldBe 2
    }
  }

  test("POST returns json with valid hashes") {
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[JsonSignedPayload].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile".refineTry[ValidTargetFilename].get).hashes(HashMethod.SHA256) shouldBe testFile.checksum.hash
    }
  }

  test("POSTing a file adds uri to custom field") {
    val urlTestFile = testFile.copy(
      uri = Uri("https://ats.com/urlTestFile"),
      name = TargetName("myfilewithuri").some,
      version = TargetVersion("0.1.0").some,
      targetFormat = None
    )

    Post(apiUri(s"repo/${repoId.show}/targets/myfilewithuri"), urlTestFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[JsonSignedPayload].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      val item = targetsRole.targets("myfilewithuri".refineTry[ValidTargetFilename].get)

      item.customParsed[TargetCustom].flatMap(_.uri).map(_.toString) should contain(urlTestFile.uri.toString())
      item.customParsed[TargetCustom].flatMap(_.targetFormat).get shouldBe TargetFormat.BINARY
    }
  }

  test("fails if there is no root.json available") {
    val unexistingRepoId = RepoId.generate()

    Post(apiUri(s"repo/${unexistingRepoId.show}/targets/otherfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.FailedDependency
    }
  }

  test("GET for each role type returns the signed json with valid signatures") {
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    forAll(RoleType.ALL.reverse) { roleType =>
      Get(apiUri(s"repo/${repoId.show}/$roleType.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK

        val signedPayload = responseAs[JsonSignedPayload]
        signaturesShouldBeValid(repoId, roleType, signedPayload)
      }
    }
  }

  test("GET on timestamp.json returns a valid Timestamp role") {
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.TIMESTAMP, responseAs[SignedPayload[TimestampRole]].asJsonSignedPayload)
    }
  }

  test("GET on snapshot.json returns a valid Snapshot role") {
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.SNAPSHOT, responseAs[SignedPayload[SnapshotRole]].asJsonSignedPayload)
    }
  }

  test("GET on targets.json returns a valid Targets role") {
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      header("x-ats-role-checksum") shouldBe defined
      signaturesShouldBeValid(newRepoId, RoleType.TARGETS, responseAs[SignedPayload[TargetsRole]].asJsonSignedPayload)
    }
  }

  keyTypeTest("GET on root.json returns a valid Root role") { keyType =>
    val newRepoId = addTargetToRepo(keyType = keyType)

    Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.ROOT, responseAs[SignedPayload[RootRole]].asJsonSignedPayload)
    }
  }

  test("GET on root.json fails if not available on keyserver") {
    val newRepoId = RepoId.generate()

    Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.FailedDependency
    }
  }

  test("GET on root.json gets json from keyserver") {
    val newRepoId = RepoId.generate()

    fakeKeyserverClient.createRoot(newRepoId).futureValue

    Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.ROOT, responseAs[SignedPayload[RootRole]].asJsonSignedPayload)
      header("x-ats-tuf-repo-id").get.value() shouldBe newRepoId.uuid.toString
    }
  }

  test("POST a new target updates snapshot.json") {
    val snapshotRole =
      Get(apiUri(s"repo/${repoId.show}/snapshot.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[SnapshotRole]]
      }

    Future { Thread.sleep(1100) }.futureValue

    Post(apiUri(s"repo/${repoId.show}/targets/changesnapshot"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val newTimestampRole =
      Get(apiUri(s"repo/${repoId.show}/snapshot.json"), testFile) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[SnapshotRole]]
      }

    snapshotRole.signed.expires.isBefore(newTimestampRole.signed.expires) shouldBe true

    snapshotRole.signatures.head shouldNot be(newTimestampRole.signatures.head)
  }

  test("POST a new target updates timestamp.json") {
    val timestampRole =
      Get(apiUri(s"repo/${repoId.show}/timestamp.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[TimestampRole]]
      }

    Future { Thread.sleep(1100) }.futureValue

    Post(apiUri(s"repo/${repoId.show}/targets/changets"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val newTimestampRole =
      Get(apiUri(s"repo/${repoId.show}/timestamp.json"), testFile) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[TimestampRole]]
      }

    timestampRole.signed.expires.isBefore(newTimestampRole.signed.expires) shouldBe true

    timestampRole.signatures.head shouldNot be(newTimestampRole.signatures.head)
  }

  test("timestamp.json is refreshed if expired") {
    val role = Get(apiUri(s"repo/${repoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[TimestampRole]]
    }

    val newRole = SignedRole.withChecksum[TimestampRole](repoId, role.asJsonSignedPayload, role.signed.version, Instant.now.minus(1, ChronoUnit.DAYS))
    signedRoleRepository.update(newRole).futureValue

    Get(apiUri(s"repo/${repoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val updatedRole = responseAs[SignedPayload[TimestampRole]].signed

      updatedRole.version shouldBe role.signed.version + 1
      updatedRole.expires.isAfter(Instant.now) shouldBe true
    }
  }

  test("snapshot.json is refreshed if expired") {
    val role = Get(apiUri(s"repo/${repoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[SnapshotRole]]
    }

    val newRole = SignedRole.withChecksum[SnapshotRole](repoId, role.asJsonSignedPayload, role.signed.version, Instant.now.minus(1, ChronoUnit.DAYS))
    signedRoleRepository.update(newRole).futureValue

    Get(apiUri(s"repo/${repoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val updatedRole = responseAs[SignedPayload[SnapshotRole]].signed

      updatedRole.version shouldBe role.signed.version + 1
      updatedRole.expires.isAfter(Instant.now) shouldBe true
    }
  }

  test("targets.json is refreshed if expired") {
    val role = Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[TargetsRole]]
    }

    val newRole = SignedRole.withChecksum[TargetsRole](repoId, role.asJsonSignedPayload, role.signed.version, Instant.now.minus(1, ChronoUnit.DAYS))
    signedRoleRepository.update(newRole).futureValue

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val updatedRole = responseAs[SignedPayload[TargetsRole]].signed

      updatedRole.version shouldBe role.signed.version + 1
      updatedRole.expires.isAfter(Instant.now) shouldBe true
    }
  }

  test("GET on a role returns valid json before targets are added") {
    val repoId = RepoId.generate()

    Post(apiUri(s"repo/${repoId.show}")).withHeaders(RawHeader("x-ats-namespace", repoId.show)) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[TargetsRole]].signed.targets should be(empty)
    }

    forAll(RoleType.ALL) { roleType =>
      Get(apiUri(s"repo/${repoId.show}/$roleType.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }
    }
  }

  keyTypeTest("SnapshotRole includes signed jsons lengths") { keyType =>
    val newRepoId = addTargetToRepo(keyType = keyType)

    val targetsRole =
      Get(apiUri(s"repo/${newRepoId.show}/targets.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[TargetsRole]]
      }

    val rootRole =
      Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[RootRole]]
      }

    Get(apiUri(s"repo/${newRepoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val signed = responseAs[SignedPayload[SnapshotRole]].signed

      val targetLength = targetsRole.asJson.canonical.length
      signed.meta(RoleType.TARGETS.metaPath).length shouldBe targetLength

      val rootLength = rootRole.asJson.canonical.length
      signed.meta(RoleType.ROOT.metaPath).length shouldBe rootLength
    }
  }

  test("GET snapshots.json returns json with valid hashes") {
    val newRepoId = addTargetToRepo()

    Post(apiUri(s"repo/${newRepoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${newRepoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targetsRole = responseAs[SignedPayload[TargetsRole]]

      val targetsCheckSum = Sha256Digest.digest(targetsRole.asJson.canonical.getBytes)

      Get(apiUri(s"repo/${newRepoId.show}/snapshot.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        val snapshotRole = responseAs[SignedPayload[SnapshotRole]].signed

        val hash = snapshotRole.meta(RoleType.TARGETS.metaPath).hashes(targetsCheckSum.method)

        hash shouldBe targetsCheckSum.hash
      }
    }
  }

  test("fails for non existent targets") {
    val newRepoId = addTargetToRepo()

    Delete(apiUri(s"repo/${newRepoId.show}/targets/doesnotexist")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("delete removes target item from targets.json") {
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targetsRole = responseAs[SignedPayload[TargetsRole]]

      targetsRole.signed.targets shouldNot be(empty)
    }

    Delete(apiUri(s"repo/${newRepoId.show}/targets/myfile01")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"repo/${newRepoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targetsRole = responseAs[SignedPayload[TargetsRole]]

      targetsRole.signed.targets should be(empty)
    }
  }

  test("delete removes target from target store when target is managed") {
    val repoId = addTargetToRepo()

    Put(apiUri(s"repo/${repoId.show}/targets/some/target?name=bananas&version=0.0.1"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val targetFilename = refineV[ValidTargetFilename]("some/target").right.get

    localStorage.retrieve(repoId, targetFilename).futureValue shouldBe a[TargetBytes]

    Delete(apiUri(s"repo/${repoId.show}/targets/some/target")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"repo/${repoId.show}/targets/some/target")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }

    localStorage.retrieve(repoId, targetFilename).failed.futureValue shouldBe Errors.TargetNotFoundError
  }

  test("delete fails for offline signed targets.json") {
    val repoId = addTargetToRepo()
    val root = fakeKeyserverClient.fetchRootRole(repoId).futureValue

    fakeKeyserverClient.deletePrivateKey(repoId, root.signed.roles(RoleType.TARGETS).keyids.head).futureValue

    Delete(apiUri(s"repo/${repoId.show}/targets/myfile01")) ~> routes ~> check {
      status shouldBe StatusCodes.PreconditionFailed
      responseAs[ErrorRepresentation].code shouldBe KeyserverClient.RoleKeyNotFound.code
    }
  }

  test("Bumps version number when adding a new target") {
    val newRepoId = addTargetToRepo()

    Post(apiUri(s"repo/${newRepoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${newRepoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targetsRole = responseAs[SignedPayload[SnapshotRole]]

      targetsRole.signed.version shouldBe 2
    }

    Get(apiUri(s"repo/${newRepoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targetsRole = responseAs[SignedPayload[TargetsRole]]

      targetsRole.signed.version shouldBe 2
    }

    Get(apiUri(s"repo/${newRepoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val role = responseAs[SignedPayload[TimestampRole]]
      role.signed.version shouldBe 2
    }
  }

  keyTypeTest("delegates to keyServer to create root") { keyType =>
    val newRepoId = RepoId.generate()

    withRandomNamepace { implicit ns =>
      Post(apiUri(s"repo/${newRepoId.show}"), CreateRepositoryRequest(keyType)).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
        fakeKeyserverClient.fetchRootRole(newRepoId).futureValue.signed shouldBe a[RootRole]
      }
    }
  }

  keyTypeTest("POST on user_create creates a repository for a namespace") { keyType =>
    withRandomNamepace { implicit ns =>
      Post(apiUri(s"user_repo"), CreateRepositoryRequest(keyType)).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
        val newRepoId = responseAs[RepoId]
        fakeKeyserverClient.fetchRootRole(newRepoId).futureValue.signed shouldBe a[RootRole]
      }
    }
  }

  keyTypeTest("POST on user_create creates a repository for a namespace with given KeyType") { keyType =>
    val otherKeyType = if (keyType == Ed25519KeyType) RsaKeyType else Ed25519KeyType

    withRandomNamepace { implicit ns =>
      Post(apiUri("user_repo"), CreateRepositoryRequest(otherKeyType)).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
        val newRepoId = responseAs[RepoId]
        val signed = fakeKeyserverClient.fetchRootRole(newRepoId).futureValue.signed
        signed shouldBe a[RootRole]

        signed.roles(RoleType.ROOT).keyids.foreach(keyId => assert(signed.keys(keyId).keytype == otherKeyType))
      }
    }
  }

  keyTypeTest("creating a target on user_creates adds target to user repo") { keyType =>
    withRandomNamepace { implicit ns =>
      val newRepoId = Post(apiUri(s"user_repo"), CreateRepositoryRequest(keyType)).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RepoId]
      }

      Post(apiUri("user_repo/targets/myfile"), testFile).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK

        val signedPayload = responseAs[JsonSignedPayload]
        signaturesShouldBeValid(newRepoId, RoleType.TARGETS, signedPayload)
      }
    }
  }

  keyTypeTest("getting role after adding a target on user repo returns user role") { keyType =>
    withRandomNamepace { implicit ns =>
      val newRepoId = Post(apiUri("user_repo"), CreateRepositoryRequest(keyType)).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RepoId]
      }

      Post(apiUri("user_repo/targets/myfile"), testFile).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK

        val signedPayload = responseAs[JsonSignedPayload]
        signaturesShouldBeValid(newRepoId, RoleType.TARGETS, signedPayload)
      }

      Get(apiUri("user_repo/root.json"), testFile).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK

        val signedPayload = responseAs[SignedPayload[RootRole]]
        signaturesShouldBeValid(newRepoId, RoleType.ROOT, signedPayload.asJsonSignedPayload)
      }
    }
  }

  keyTypeTest("fails if repo for user already exists") { keyType =>
    withRandomNamepace { implicit ns =>
      Post(apiUri("user_repo"), CreateRepositoryRequest(keyType)).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }

      Post(apiUri("user_repo")).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.Conflict
      }
    }
  }

  test("creating repo fails for invalid key type parameter") {
    withRandomNamepace { implicit ns =>
      Post(apiUri("user_repo"))
          .withEntity(ContentTypes.`application/json`, """ { "keyType":"caesar" } """)
          .namespaced ~> routes ~> check {
        status shouldBe StatusCodes.BadRequest
      }
    }
  }

  val testEntity = HttpEntity(ByteString("""
                                           |Like all the men of the Library, in my younger days I traveled;
                                           |I have journeyed in quest of a book, perhaps the catalog of catalogs.
                                           |""".stripMargin))

  val fileBodyPart = BodyPart("file", testEntity, Map("filename" -> "babel.txt"))

  val form = Multipart.FormData(fileBodyPart)


  test("uploading a target changes targets json") {
    val repoId = addTargetToRepo()

    Put(apiUri(s"repo/${repoId.show}/targets/some/target/funky/thing?name=name&version=version"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/targets/some/target/funky/thing")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseEntity.dataBytes.runReduce(_ ++ _).futureValue shouldBe testEntity.getData()
    }
  }

  keyTypeTest("uploading a target from a uri changes targets json") { keyType =>
    val repoId = addTargetToRepo(keyType = keyType)

    Put(apiUri(s"repo/${repoId.show}/targets/some/target/funky/thing?name=name&version=version&fileUri=${fakeHttpClient.fileUri}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/targets/some/target/funky/thing")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseEntity.dataBytes.runReduce(_ ++ _).futureValue shouldBe fakeHttpClient.fileBody.getData()
    }
  }

  test("returns 404 if target does not exist") {
    val repoId = addTargetToRepo()

    Get(apiUri(s"repo/${repoId.show}/targets/some/thing")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("accept name/version, hardwareIds, targetFormat") {
    val repoId = addTargetToRepo()
    val targetFilename: TargetFilename = Refined.unsafeApply("target/with/desc")

    Put(apiUri(s"repo/${repoId.show}/targets/${targetFilename.value}?name=somename&version=someversion&hardwareIds=1,2,3&targetFormat=binary"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val custom = responseAs[SignedPayload[TargetsRole]].signed.targets(targetFilename).customParsed[TargetCustom]

      custom.map(_.name) should contain(TargetName("somename"))
      custom.map(_.version) should contain(TargetVersion("someversion"))
      custom.map(_.hardwareIds.map(_.value)) should contain(Seq("1", "2", "3"))
      custom.flatMap(_.targetFormat) should contain(TargetFormat.BINARY)
    }
  }

  test("Missing targetFormat gets set to BINARY") {
    val repoId = addTargetToRepo()
    val targetFilename: TargetFilename = Refined.unsafeApply("target/with/desc")

    Put(apiUri(s"repo/${repoId.show}/targets/${targetFilename.value}?name=somename&version=someversion&hardwareIds=1,2,3"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val custom = responseAs[SignedPayload[TargetsRole]].signed.targets(targetFilename).customParsed[TargetCustom]

      custom.flatMap(_.targetFormat) should contain(TargetFormat.BINARY)
    }
  }

  test("on updates, updatedAt in target custom is updated, createdAt is unchanged") {
    val repoId = addTargetToRepo()
    val targetFilename: TargetFilename = Refined.unsafeApply("target/to/update")

    Put(apiUri(s"repo/${repoId.show}/targets/${targetFilename.value}?name=somename&version=someversion"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val now = Instant.now

    Thread.sleep(1000)

    Put(apiUri(s"repo/${repoId.show}/targets/${targetFilename.value}?name=somename&version=someversion"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val custom = responseAs[SignedPayload[TargetsRole]].signed.targets(targetFilename).customParsed[TargetCustom]

      custom.map(_.createdAt).get.isBefore(now) shouldBe true
      custom.map(_.updatedAt).get.isAfter(now) shouldBe true
    }
  }

  test("create a repo returns 409 if repo for namespace already exists") {
    val repoId = RepoId.generate()

    Post(apiUri(s"repo/${repoId.show}")).withHeaders(RawHeader("x-ats-namespace", repoId.show)) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    fakeKeyserverClient.fetchRootRole(repoId).futureValue shouldBe a[SignedPayload[_]]

    val otherRepoId = RepoId.generate()

    Post(apiUri(s"repo/${otherRepoId.show}")).withHeaders(RawHeader("x-ats-namespace", repoId.show)) ~> routes ~> check {
      status shouldBe StatusCodes.Conflict
    }

    fakeKeyserverClient.fetchRootRole(otherRepoId).failed.futureValue.asInstanceOf[RawError].code.code shouldBe "root_role_not_found"
  }

  keyTypeTest("accepts an offline signed targets.json") { keyType =>
    val repoId = addTargetToRepo(keyType = keyType)

    Put(apiUri(s"repo/${repoId.show}/targets/old/target?name=bananas&version=0.0.1"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val targetFilename = refineV[ValidTargetFilename]("old/target").right.get
    localStorage.retrieve(repoId, targetFilename).futureValue shouldBe a[TargetRetrieveResult]

    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets, version = 3)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
      header("x-ats-role-checksum").map(_.value) should contain(makeRoleChecksumHeader(repoId).value)
    }

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targets = responseAs[SignedPayload[TargetsRole]].signed.targets
      targets.keys should contain(offlineTargetFilename)
    }

    // check that previous target has been deleted
    localStorage.retrieve(repoId, targetFilename).failed.futureValue shouldBe Errors.TargetNotFoundError
  }

  test("reject putting offline signed targets.json without checksum if it exists already") {
    val repoId = addTargetToRepo()
    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.PreconditionRequired
    }
  }

  test("getting offline target item fails if no custom url was provided when signing target") {
    val repoId = addTargetToRepo()
    val targetCustomJson =TargetCustom(TargetName("name"), TargetVersion("version"), Seq.empty, TargetFormat.BINARY.some).asJson
    val hashes: ClientHashes = Map(HashMethod.SHA256 -> Refined.unsafeApply("8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"))
    val offlineTargets = Map(offlineTargetFilename -> ClientTargetItem(hashes, 0, targetCustomJson.some))

    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"repo/${repoId.show}/targets/${offlineTargetFilename.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.ExpectationFailed
      responseAs[ErrorRepresentation].code shouldBe ErrorCodes.NoUriForUnmanagedTarget
    }
  }

  test("getting offline target item redirects to custom url") {
    val repoId = addTargetToRepo()

    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"repo/${repoId.show}/targets/${offlineTargetFilename.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.Found
      header[Location].map(_.value()) should contain("https://ats.com")
    }
  }

  test("POST /targets fails with 412 with offline targets.json") {
    val repoId = RepoId.generate()
    fakeKeyserverClient.createRoot(repoId).futureValue

    val root = fakeKeyserverClient.fetchRootRole(repoId).futureValue

    fakeKeyserverClient.deletePrivateKey(repoId, root.signed.roles(RoleType.TARGETS).keyids.head).futureValue

    Post(apiUri(s"repo/${repoId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.PreconditionFailed
    }
  }

  test("re-generates snapshot role after storing offline target") {
    val repoId = addTargetToRepo()

    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"repo/${repoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[SnapshotRole]].signed.version shouldBe 2
    }
  }

  test("re-generates timestamp role after storing offline target") {
    val repoId = addTargetToRepo()

    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"repo/${repoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[TimestampRole]].signed.version shouldBe 2
    }
  }

  test("PUT offline target fails when target does not include custom meta") {
    val repoId = addTargetToRepo()

    val targets = Map(offlineTargetFilename -> ClientTargetItem(Map.empty, 0, None))
    val signedPayload = buildSignedTargetsRole(repoId, targets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("target item error some/file/name: new offline signed target items must contain custom metadata")
    }
  }

  test("rejects requests with invalid/missing checksums") {
    val clientTargetItem = offlineTargets(offlineTargetFilename).copy(hashes = Map.empty)
    val targets = Map(offlineTargetFilename -> clientTargetItem)
    val signedPayload = buildSignedTargetsRole(repoId, targets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("Invalid/Missing Checksum")
    }
  }

  test("accepts requests with no uri in target custom") {
    val repoId = addTargetToRepo()

    val targetCustomJson = TargetCustom(TargetName("name"), TargetVersion("version"), Seq.empty, TargetFormat.BINARY.some).asJson

    val hashes: ClientHashes = Map(HashMethod.SHA256 -> Refined.unsafeApply("8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"))
    val targets = Map(offlineTargetFilename -> ClientTargetItem(hashes, 0, targetCustomJson.some))
    val signedPayload = buildSignedTargetsRole(repoId, targets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }
  }

  keyTypeTest("rejects offline targets.json with bad signatures") { keyType =>
    val repoId = addTargetToRepo(keyType = keyType)

    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), Map.empty, 2)

    val invalidSignedPayload = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, "something else signed".asJson).futureValue

    val signedPayload = JsonSignedPayload(invalidSignedPayload.signatures, targetsRole.asJson)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("Invalid signature for key")
    }
  }

  keyTypeTest("rejects offline targets.json with less signatures than the required threshold") { keyType =>
    val repoId = addTargetToRepo(keyType = keyType)

    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), Map.empty, 2)

    val signedPayload = JsonSignedPayload(Seq.empty, targetsRole.asJson)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("Valid signature count must be >= threshold")
    }
  }

  test("rejects offline targets.json if public keys are not available") {
    val repoId = addTargetToRepo()

    val targetFilename: TargetFilename = Refined.unsafeApply("some/file/name")
    val targets = Map(targetFilename -> ClientTargetItem(Map.empty, 0, None))

    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), targets, 2)

    val Ed25519TufKeyPair(pub, sec) = TufCrypto.generateKeyPair(Ed25519KeyType, 256)
    val signature = TufCrypto.signPayload(sec, targetsRole.asJson).toClient(pub.id)
    val signedPayload = JsonSignedPayload(List(signature), targetsRole.asJson)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include(s"key ${pub.id} required for role validation not found in root role")
    }
  }

  test("return offline targets.json even if expired") {
    val repoId = addTargetToRepo()

    val expiredTargetsRole = TargetsRole(Instant.now().minus(1, ChronoUnit.DAYS), offlineTargets, 2)
    val signedPayload = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, expiredTargetsRole.asJson).futureValue

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    // delete the key to simulate offline targets.json
    fakeKeyserverClient.deletePrivateKey(repoId, signedPayload.signatures.head.keyid).futureValue

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  test("fake keyserver client saves instants with same precision as json codecs") {
    val newRepoId = RepoId.generate()
    fakeKeyserverClient.createRoot(newRepoId).futureValue
    val raw = fakeKeyserverClient.fetchRootRole(newRepoId).futureValue.signed
    val rawJson = fakeKeyserverClient.fetchRootRole(newRepoId).futureValue.signed.asJson.as[RootRole].valueOr(throw _)

    raw.expires shouldBe rawJson.expires
  }

  keyTypeTest("delegates getting specific version of root.json to keyserver") { keyType =>
    val newRepoId = RepoId.generate()

    withRandomNamepace { implicit ns =>
      Post(apiUri(s"repo/${newRepoId.show}"), CreateRepositoryRequest(keyType)).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }
    }

    val newRoot = Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]]
    }

    Get(apiUri(s"repo/${newRepoId.show}/1.root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed shouldBe newRoot.signed
    }
  }

  def signaturesShouldBeValid(repoId: RepoId, roleType: RoleType, signedPayload: JsonSignedPayload): Assertion = {
    val signature = signedPayload.signatures.head
    val signed = signedPayload.signed

    val isValid = TufCrypto.isValid(signature, fakeKeyserverClient.publicKey(repoId, roleType), signed)
    isValid shouldBe true
  }

}

// Test for message publishing in separate class
// because MemoryMessageBus maintains queue and supports only single subscriber.
//
class RepoResourceTufTargetSpec(keyType: KeyType) extends TufReposerverSpec
  with ResourceSpec with PatienceConfiguration with RepoSupport {

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  test("publishes messages to bus on adding target") {
    val repoId = RepoId.generate()
    val source = memoryMessageBus.subscribe[TufTargetAdded]()

    withNamespace(s"default-${repoId.show}") { implicit ns =>
      Post(apiUri(s"repo/${repoId.show}"), CreateRepositoryRequest(keyType)).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }

      Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }

      source.runWith(Sink.head).futureValue shouldBe a[TufTargetAdded]
    }
  }

}

class RsaRepoResourceTufTargetSpec extends RepoResourceTufTargetSpec(RsaKeyType)
class Ed25519RepoResourceTufTargetSpec extends RepoResourceTufTargetSpec(Ed25519KeyType)

class RepoResourceTufTargetInitialJsonSpec(keyType: KeyType) extends TufReposerverSpec
  with ResourceSpec with PatienceConfiguration with RepoSupport {

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  test("publishes messages to bus on creating first target by uploading target.json") {
    val sink = memoryMessageBus.subscribe[TufTargetAdded]().runWith(TestSink.probe[TufTargetAdded])
    val repoId = RepoId.generate()

    withNamespace(s"default-${repoId.show}") { implicit ns =>
      Post(apiUri(s"repo/${repoId.show}")).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }

      val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

      Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload) ~> routes ~> check {
        status shouldBe StatusCodes.NoContent
        header("x-ats-role-checksum").map(_.value) should contain(makeRoleChecksumHeader(repoId).value)
      }

      sink.requestNext()
    }
  }
}

class RsaRepoResourceTufTargetInitialJsonSpec extends RepoResourceTufTargetInitialJsonSpec(RsaKeyType)
class Ed25519RepoResourceTufTargetInitialJsonSpec extends RepoResourceTufTargetInitialJsonSpec(Ed25519KeyType)

class RepoResourceTufTargetJsonSpec(keyType: KeyType) extends TufReposerverSpec
  with ResourceSpec with PatienceConfiguration with RepoSupport {

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  test("publishes messages to bus for new target entries") {
    val sink = memoryMessageBus.subscribe[TufTargetAdded]().runWith(TestSink.probe[TufTargetAdded])

    val repoId = addTargetToRepo()
    // check for target item
    sink.requestNext()

    val signedPayload1 = buildSignedTargetsRole(repoId, offlineTargets, version = 2)

    // 2nd target item
    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload1).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
      header("x-ats-role-checksum").map(_.value) should contain(makeRoleChecksumHeader(repoId).value)
    }

    sink.requestNext()

    val targetRole = Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targetR = responseAs[SignedPayload[TargetsRole]].signed
      targetR.targets.keys should contain(offlineTargetFilename)
      header("x-ats-role-checksum").map(_.value) should contain(makeRoleChecksumHeader(repoId).value)
      targetR
    }

    // 3rd target item
    val hashes: ClientHashes = Map(HashMethod.SHA256 -> Refined.unsafeApply("8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"))
    val targetCustomJson = TargetCustom(TargetName("name"), TargetVersion("version"), Seq.empty, TargetFormat.BINARY.some)
                              .asJson
                              .deepMerge(Json.obj("uri" -> Uri("https://ats.com").asJson))
    val offlineTargetFilename2: TargetFilename = Refined.unsafeApply("another/file/name")
    val offlineTargets2 = targetRole.targets.updated(offlineTargetFilename2, ClientTargetItem(hashes, 0, Some(targetCustomJson)))
    val signedPayload2 = buildSignedTargetsRole(repoId, offlineTargets2, targetRole.version + 1)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload2).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      header("x-ats-role-checksum").map(_.value) should contain(makeRoleChecksumHeader(repoId).value)
      status shouldBe StatusCodes.NoContent
    }

    sink.requestNext()
  }

}

class RsaRepoResourceTufTargetJsonSpec extends RepoResourceTufTargetJsonSpec(RsaKeyType)

class EdRepoResourceTufTargetJsonSpec extends RepoResourceTufTargetJsonSpec(Ed25519KeyType)

class RepoResourceTufTargetStoreSpec(keyType: KeyType) extends TufReposerverSpec
    with ResourceSpec with Inspectors with Whenever with PatienceConfiguration {

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  val testEntity = HttpEntity(ByteString("""
                                           |Like all the men of the Library, in my younger days I traveled;
                                           |I have journeyed in quest of a book, perhaps the catalog of catalogs.
                                           |""".stripMargin))

  val fileBodyPart = BodyPart("file", testEntity, Map("filename" -> "babel.txt"))

  val form = Multipart.FormData(fileBodyPart)

  test("publishes usage messages to bus") {
    val repoId = RepoId.generate()
    val source = memoryMessageBus.subscribe[PackageStorageUsage]()

    withNamespace(s"default-${repoId.show}") { implicit ns =>
      Post(apiUri(s"repo/${repoId.show}"), CreateRepositoryRequest(keyType)).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }

      Put(apiUri(s"repo/${repoId.show}/targets/some/file?name=pkgname&version=pkgversion&desc=wat"), form).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }

      val messages = source.take(2).runWith(Sink.seq).futureValue
      messages.head shouldBe a[PackageStorageUsage]
      messages.last shouldBe a[TufTargetAdded]
    }
  }
}

class RsaRepoResourceTufTargetStoreSpec extends RepoResourceTufTargetStoreSpec(RsaKeyType)

class EdRepoResourceTufTargetStoreSpec extends RepoResourceTufTargetStoreSpec(Ed25519KeyType)

class RepoResourceCommentSpec extends TufReposerverSpec with ResourceSpec with PatienceConfiguration with RepoSupport
{

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  test("set comment for existing repo id and package") {
    val repoId = addTargetToRepo()

    Put(apiUri(s"repo/${repoId.show}/comments/myfile01"), CommentRequest(TargetComment("comment"))) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  test("set comment for existing repo id and non-existing package") {
    val repoId = RepoId.generate()
    fakeKeyserverClient.createRoot(repoId).futureValue

    Put(apiUri(s"repo/${repoId.show}/comments/myfile01"), CommentRequest(TargetComment("comment"))) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("set comment for non-existing repo id and non-existing package") {
    val repoId = RepoId.generate()

    Put(apiUri(s"repo/${repoId.show}/comments/myfile01"), CommentRequest(TargetComment("comment"))) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("get existing comment") {
    val repoId = addTargetToRepo()

    Put(apiUri(s"repo/${repoId.show}/comments/myfile01"), CommentRequest(TargetComment("ಠ_ಠ"))) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/comments/myfile01")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      entityAs[CommentRequest] shouldBe CommentRequest(TargetComment("ಠ_ಠ"))
    }
  }

  test("getting comment of deleted package fails") {
    val repoId = addTargetToRepo()

    Put(apiUri(s"repo/${repoId.show}/comments/myfile01"), CommentRequest(TargetComment("ಠ_ಠ"))) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/comments/myfile01")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      entityAs[CommentRequest] shouldBe CommentRequest(TargetComment("ಠ_ಠ"))
    }

    Delete(apiUri(s"repo/${repoId.show}/targets/myfile01")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"repo/${repoId.show}/comments/myfile01")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("trying to get missing comment for existing repo id and package returns 404") {
    val repoId = addTargetToRepo()

    Get(apiUri(s"repo/${repoId.show}/comments/myfile01")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("trying to get non-existing comment list for existing repo id") {
    val repoId = addTargetToRepo()

    Get(apiUri(s"repo/${repoId.show}/comments")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("trying to get comment list for non-existing repo id") {
    val repoId = RepoId.generate()

    Get(apiUri(s"repo/${repoId.show}/comments")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("get existing comment list") {
    val repoId = addTargetToRepo()

    Put(apiUri(s"repo/${repoId.show}/comments/myfile01"), CommentRequest(TargetComment("comment"))) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/comments")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      entityAs[Seq[FilenameComment]] shouldBe List(FilenameComment("myfile01".refineTry[ValidTargetFilename].get,
        TargetComment("comment")))
    }
  }

  test("get existing comment list for different versions") {
    val repoId = RepoId.generate()
    fakeKeyserverClient.createRoot(repoId, RsaKeyType).futureValue
    val repoIdS = repoId.show

    Post(apiUri(s"repo/$repoIdS/targets/raspberrypi_rocko-ce15f3986223be401205d13dda6e8d7aefeae1c02a769043ba11d1268ccd77dd"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Put(apiUri(s"repo/$repoIdS/comments/raspberrypi_rocko-ce15f3986223be401205d13dda6e8d7aefeae1c02a769043ba11d1268ccd77dd"),
                        CommentRequest(TargetComment("comment1"))) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val testFile2 = {
      val checksum = Sha256Digest.digest("lo".getBytes)
      RequestTargetItem(Uri("https://ats.com/testfile"), checksum, targetFormat = None, name = None, version = None,
                            hardwareIds = Seq.empty, length = "lo".getBytes.length)
    }

    Post(apiUri(s"repo/$repoIdS/targets/raspberrypi_rocko-d359911e6fb67476e379a55870d1a180acc3a78d6d463b5281ccd9ca861519dc"), testFile2) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Put(apiUri(s"repo/$repoIdS/comments/raspberrypi_rocko-d359911e6fb67476e379a55870d1a180acc3a78d6d463b5281ccd9ca861519dc"),
                          CommentRequest(TargetComment("comment2"))) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/$repoIdS/comments")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      entityAs[Seq[FilenameComment]].length shouldBe 2
    }
  }

  keyTypeTest("updating targets.json doesn't kill comments") { keyType =>
    val repoId = addTargetToRepo(keyType = keyType)
    val repoIdS = repoId.show

    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets, version = 2)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
      header("x-ats-role-checksum").map(_.value) should contain(makeRoleChecksumHeader(repoId).value)
    }

    Put(apiUri(s"repo/$repoIdS/comments/$offlineTargetFilename"),
      CommentRequest(TargetComment("comment1"))) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/$repoIdS/comments/$offlineTargetFilename")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      entityAs[CommentRequest] shouldBe CommentRequest(TargetComment("comment1"))
    }

    val signedPayload2 = buildSignedTargetsRole(repoId, offlineTargets, version = 3)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload2).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
      header("x-ats-role-checksum").map(_.value) should contain(makeRoleChecksumHeader(repoId).value)
    }

    Get(apiUri(s"repo/$repoIdS/comments/$offlineTargetFilename")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      entityAs[CommentRequest] shouldBe CommentRequest(TargetComment("comment1"))
    }

  }

}

object JsonErrors {
  import io.circe.generic.semiauto._
  implicit val jsonErrorsEncoder: Encoder[JsonErrors] = deriveEncoder
  implicit val jsonErrorsDecoder: Decoder[JsonErrors] = deriveDecoder
}

case class JsonErrors(errors: NonEmptyList[String]) {
  def head: String = errors.head
}
