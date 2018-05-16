package com.advancedtelematic.tuf.reposerver.http

import java.time.Instant
import java.time.temporal.ChronoUnit

import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.{MalformedRequestContentRejection, RejectionHandler}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.scaladsl.Sink
import akka.stream.testkit.scaladsl.TestSink
import akka.util.ByteString
import cats.data.NonEmptyList
import cats.syntax.show._
import cats.syntax.option._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientHashes, ClientTargetItem, RootRole, SnapshotRole, TargetCustom, TargetsRole, TimestampRole}
import com.advancedtelematic.libtuf.data.ClientDataType.RoleTypeOps
import com.advancedtelematic.libtuf_server.data.Messages.{PackageStorageUsage, TufTargetAdded}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, _}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, ParsingFailure}
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import org.scalatest.prop.Whenever
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{Assertion, BeforeAndAfterAll, Inspectors, Suite}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import com.advancedtelematic.libats.codecs.CirceCodecs._
import com.advancedtelematic.libats.http.HttpCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import cats.syntax.either._
import com.advancedtelematic.libats.codecs.{DeserializationException, RefinementError}
import com.advancedtelematic.libats.data.DataType.HashMethod
import com.advancedtelematic.libats.data.ErrorRepresentation
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf_server.crypto.Sha256Digest
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient.RequestTargetItem
import com.advancedtelematic.tuf.reposerver.http.RepoResource.CreateRepositoryRequest
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.SignedRole
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepositorySupport
import com.advancedtelematic.tuf.reposerver.util.NamespaceSpecOps._
import com.advancedtelematic.tuf.reposerver.util._
import eu.timepit.refined.api.Refined
import scala.concurrent.Future


trait RepoSupport extends ResourceSpec with SignedRoleRepositorySupport with ScalaFutures with ScalatestRouteTest { this: Suite ⇒
  implicit val ec = executor

  def makeRoleChecksumHeader(repoId: RepoId) =
    RoleChecksumHeader(signedRoleRepo.find(repoId, RoleType.TARGETS).futureValue.checksum.hash)

  val testFile = {
    val checksum = Sha256Digest.digest("hi".getBytes)
    RequestTargetItem(Uri("https://ats.com/testfile"), checksum, targetFormat = None, name = None, version = None, hardwareIds = Seq.empty, length = "hi".getBytes.length)
  }

  def addTargetToRepo(repoId: RepoId = RepoId.generate()): RepoId = {
    fakeKeyserverClient.createRoot(repoId).futureValue

    Post(apiUri(s"repo/${repoId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      repoId
    }
  }

  def buildSignedTargetsRole(repoId: RepoId, targets: Map[TargetFilename, ClientTargetItem], version: Int = 2): SignedPayload[TargetsRole] = {
    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), targets, version)
    fakeKeyserverClient.sign(repoId, RoleType.TARGETS, targetsRole).futureValue
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

  keyTypeTest("POST returns latest signed json") { keyType =>
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[SignedPayload[Json]]
      signaturesShouldBeValid(repoId, RoleType.TARGETS, signedPayload)

      val signed = signedPayload.signed
      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile".refineTry[ValidTargetFilename].get).length shouldBe 2
    }
  }

  keyTypeTest("POST returns json with previous elements") { keyType =>
    Post(apiUri(s"repo/${repoId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Post(apiUri(s"repo/${repoId.show}/targets/myfile02"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[SignedPayload[Json]].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile01".refineTry[ValidTargetFilename].get).length shouldBe 2
      targetsRole.targets("myfile02".refineTry[ValidTargetFilename].get).length shouldBe 2
    }
  }

  keyTypeTest("POST returns json with valid hashes") { keyType =>
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[SignedPayload[Json]].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile".refineTry[ValidTargetFilename].get).hashes(HashMethod.SHA256) shouldBe testFile.checksum.hash
    }
  }

  keyTypeTest("POSTing a file adds uri to custom field") { keyType =>
    val urlTestFile = testFile.copy(
      uri = Uri("https://ats.com/urlTestFile"),
      name = TargetName("myfilewithuri").some,
      version = TargetVersion("0.1.0").some
    )

    Post(apiUri(s"repo/${repoId.show}/targets/myfilewithuri"), urlTestFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[SignedPayload[Json]].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      val item = targetsRole.targets("myfilewithuri".refineTry[ValidTargetFilename].get)

      item.customParsed[TargetCustom].flatMap(_.uri).map(_.toString) should contain(urlTestFile.uri.toString())
    }
  }

  keyTypeTest("fails if there is no root.json available") { keyType =>
    val unexistingRepoId = RepoId.generate()

    Post(apiUri(s"repo/${unexistingRepoId.show}/targets/otherfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.FailedDependency
    }
  }

  keyTypeTest("GET for each role type returns the signed json with valid signatures") { keyType =>
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    forAll(RoleType.ALL.reverse) { roleType =>
      Get(apiUri(s"repo/${repoId.show}/$roleType.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK

        val signedPayload = responseAs[SignedPayload[Json]]
        signaturesShouldBeValid(repoId, roleType, signedPayload)
      }
    }
  }

  keyTypeTest("GET on timestamp.json returns a valid Timestamp role") { keyType =>
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.TIMESTAMP, responseAs[SignedPayload[TimestampRole]])
    }
  }

  keyTypeTest("GET on snapshot.json returns a valid Snapshot role") { keyType =>
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.SNAPSHOT, responseAs[SignedPayload[SnapshotRole]])
    }
  }

  keyTypeTest("GET on targets.json returns a valid Targets role") { keyType =>
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      header("x-ats-role-checksum") shouldBe defined
      signaturesShouldBeValid(newRepoId, RoleType.TARGETS, responseAs[SignedPayload[TargetsRole]])
    }
  }

  keyTypeTest("GET on root.json returns a valid Root role") { keyType =>
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.ROOT, responseAs[SignedPayload[RootRole]])
    }
  }

  keyTypeTest("GET on root.json fails if not available on keyserver") { keyType =>
    val newRepoId = RepoId.generate()

    Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.FailedDependency
    }
  }

  keyTypeTest("GET on root.json gets json from keyserver") { keyType =>
    val newRepoId = RepoId.generate()

    fakeKeyserverClient.createRoot(newRepoId).futureValue

    Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.ROOT, responseAs[SignedPayload[RootRole]])
      header("x-ats-tuf-repo-id").get.value() shouldBe newRepoId.uuid.toString
    }
  }

  keyTypeTest("POST a new target updates snapshot.json") { keyType =>
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

  keyTypeTest("POST a new target updates timestamp.json") { keyType =>
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

  keyTypeTest("timestamp.json is refreshed if expired") { keyType =>
    val role = Get(apiUri(s"repo/${repoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[TimestampRole]]
    }

    val newRole = SignedRole.withChecksum(repoId, RoleType.TIMESTAMP, role, role.signed.version, Instant.now.minus(1, ChronoUnit.DAYS))
    signedRoleRepo.update(newRole).futureValue

    Get(apiUri(s"repo/${repoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val updatedRole = responseAs[SignedPayload[TimestampRole]].signed

      updatedRole.version shouldBe role.signed.version + 1
      updatedRole.expires.isAfter(Instant.now) shouldBe true
    }
  }

  keyTypeTest("snapshot.json is refreshed if expired") { keyType =>
    val role = Get(apiUri(s"repo/${repoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[SnapshotRole]]
    }

    val newRole = SignedRole.withChecksum(repoId, RoleType.SNAPSHOT, role, role.signed.version, Instant.now.minus(1, ChronoUnit.DAYS))
    signedRoleRepo.update(newRole).futureValue

    Get(apiUri(s"repo/${repoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val updatedRole = responseAs[SignedPayload[SnapshotRole]].signed

      updatedRole.version shouldBe role.signed.version + 1
      updatedRole.expires.isAfter(Instant.now) shouldBe true
    }
  }

  keyTypeTest("targets.json is refreshed if expired") { keyType =>
    val role = Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[TargetsRole]]
    }

    val newRole = SignedRole.withChecksum(repoId, RoleType.TARGETS, role, role.signed.version, Instant.now.minus(1, ChronoUnit.DAYS))
    signedRoleRepo.update(newRole).futureValue

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val updatedRole = responseAs[SignedPayload[TargetsRole]].signed

      updatedRole.version shouldBe role.signed.version + 1
      updatedRole.expires.isAfter(Instant.now) shouldBe true
    }
  }

  keyTypeTest("GET on a role returns valid json before targets are added") { keyType =>
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
    val newRepoId = addTargetToRepo()

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
      signed.meta(RoleType.TARGETS.toMetaPath).length shouldBe targetLength

      val rootLength = rootRole.asJson.canonical.length
      signed.meta(RoleType.ROOT.toMetaPath).length shouldBe rootLength
    }
  }

  keyTypeTest("GET snapshots.json returns json with valid hashes") { keyType =>
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

        val hash = snapshotRole.meta(RoleType.TARGETS.toMetaPath).hashes(targetsCheckSum.method)

        hash shouldBe targetsCheckSum.hash
      }
    }
  }

  keyTypeTest("Bumps version number when adding a new target") { keyType =>
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

        val signedPayload = responseAs[SignedPayload[Json]]
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

        val signedPayload = responseAs[SignedPayload[Json]]
        signaturesShouldBeValid(newRepoId, RoleType.TARGETS, signedPayload)
      }

      Get(apiUri("user_repo/root.json"), testFile).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK

        val signedPayload = responseAs[SignedPayload[RootRole]]
        signaturesShouldBeValid(newRepoId, RoleType.ROOT, signedPayload)
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

  keyTypeTest("creating repo fails for invalid key type parameter") { keyType =>
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


  keyTypeTest("uploading a target changes targets json") { keyType =>
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
    val repoId = addTargetToRepo()

    Put(apiUri(s"repo/${repoId.show}/targets/some/target/funky/thing?name=name&version=version&fileUri=${fakeHttpClient.fileUri}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/targets/some/target/funky/thing")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseEntity.dataBytes.runReduce(_ ++ _).futureValue shouldBe fakeHttpClient.fileBody.getData()
    }
  }

  keyTypeTest("returns 404 if target does not exist") { keyType =>
    val repoId = addTargetToRepo()

    Get(apiUri(s"repo/${repoId.show}/targets/some/thing")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  keyTypeTest("accept name/version, hardwareIds, targetFormat") { keyType =>
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

  keyTypeTest("on updates, updatedAt in target custom is updated, createdAt is unchanged") { keyType =>
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

  keyTypeTest("create a repo returns 409 if repo for namespace already exists") { keyType =>
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
    val repoId = addTargetToRepo()
    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
      header("x-ats-role-checksum").map(_.value) should contain(makeRoleChecksumHeader(repoId).value)
    }

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targets = responseAs[SignedPayload[TargetsRole]].signed.targets
      targets.keys should contain(offlineTargetFilename)
    }
  }

  keyTypeTest("reject putting offline signed targets.json without checksum if it exists already") { keyType =>
    val repoId = addTargetToRepo()
    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.PreconditionRequired
    }
  }

  keyTypeTest("getting offline target item fails if no custom url was provided when signing target") { keyType =>
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

  keyTypeTest("getting offline target item redirects to custom url") { keyType =>
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

  keyTypeTest("POST /targets fails with 412 with offline targets.json") { keyType =>
    val repoId = RepoId.generate()
    fakeKeyserverClient.createRoot(repoId).futureValue

    val root = fakeKeyserverClient.fetchRootRole(repoId).futureValue

    fakeKeyserverClient.deletePrivateKey(repoId, root.signed.roles(RoleType.TARGETS).keyids.head).futureValue

    Post(apiUri(s"repo/${repoId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.PreconditionFailed
    }
  }

  keyTypeTest("re-generates snapshot role after storing offline target") { keyType =>
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

  keyTypeTest("re-generates timestamp role after storing offline target") { keyType =>
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


  keyTypeTest("PUT offline target fails when target does not include custom meta") { keyType =>
    val repoId = addTargetToRepo()

    val targets = Map(offlineTargetFilename -> ClientTargetItem(Map.empty, 0, None))
    val signedPayload = buildSignedTargetsRole(repoId, targets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("target item error some/file/name: new offline signed target items must contain custom metadata")
    }
  }

  keyTypeTest("rejects requests with invalid/missing checksums") { keyType =>
    val clientTargetItem = offlineTargets(offlineTargetFilename).copy(hashes = Map.empty)
    val targets = Map(offlineTargetFilename -> clientTargetItem)
    val signedPayload = buildSignedTargetsRole(repoId, targets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("Invalid/Missing Checksum")
    }
  }

  keyTypeTest("accepts requests with no uri in target custom") { keyType =>
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
    val repoId = addTargetToRepo()

    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), Map.empty, 2)

    val invalidSignedPayload = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, "something else signed").futureValue

    val signedPayload = SignedPayload(invalidSignedPayload.signatures, targetsRole)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("Invalid signature for key")
    }
  }

  keyTypeTest("rejects offline targets.json with less signatures than the required threshold") { keyType =>
    val repoId = addTargetToRepo()

    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), Map.empty, 2)

    val signedPayload = SignedPayload(Seq.empty, targetsRole)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("Valid signature count must be >= threshold")
    }
  }

  keyTypeTest("rejects offline targets.json if public keys are not available") { keyType =>
    val repoId = addTargetToRepo()

    val targetFilename: TargetFilename = Refined.unsafeApply("some/file/name")
    val targets = Map(targetFilename -> ClientTargetItem(Map.empty, 0, None))

    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), targets, 2)

    val Ed25519TufKeyPair(pub, sec) = TufCrypto.generateKeyPair(Ed25519KeyType, 256)
    val signature = TufCrypto.signPayload(sec, targetsRole).toClient(pub.id)
    val signedPayload = SignedPayload(List(signature), targetsRole)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include(s"key ${pub.id} required for role validation not found in root role")
    }
  }

  keyTypeTest("return offline targets.json even if expired") { keyType =>
    val repoId = addTargetToRepo()

    val expiredTargetsRole = TargetsRole(Instant.now().minus(1, ChronoUnit.DAYS), offlineTargets, 2)
    val signedPayload = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, expiredTargetsRole).futureValue

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(makeRoleChecksumHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    // delete the key to simulate offline targets.json
    fakeKeyserverClient.deletePrivateKey(repoId, signedPayload.signatures.head.keyid).futureValue

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  keyTypeTest("fake keyserver client saves instants with same precision as json codecs") { keyType =>
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

  def signaturesShouldBeValid[T : Encoder](repoId: RepoId, roleType: RoleType, signedPayload: SignedPayload[T]): Assertion = {
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
    with ResourceSpec  with Inspectors with Whenever with PatienceConfiguration {

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

object JsonErrors {
  import io.circe.generic.semiauto._
  implicit val jsonErrorsEncoder: Encoder[JsonErrors] = deriveEncoder
  implicit val jsonErrorsDecoder: Decoder[JsonErrors] = deriveDecoder
}

case class JsonErrors(errors: NonEmptyList[String]) {
  def head: String = errors.head
}
