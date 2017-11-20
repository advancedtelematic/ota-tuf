package com.advancedtelematic.tuf.reposerver.http

import java.time.Instant
import java.time.temporal.ChronoUnit

import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import cats.data.NonEmptyList
import cats.syntax.show.toShowOps
import cats.syntax.option._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientHashes, ClientTargetItem, RootRole, SnapshotRole, TargetCustom, TargetsRole, TimestampRole}
import com.advancedtelematic.libtuf_server.data.Messages.{PackageStorageUsage, TufTargetAdded}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, _}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.prop.Whenever
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{Assertion, BeforeAndAfterAll, Inspectors}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import com.advancedtelematic.libats.codecs.CirceCodecs._
import com.advancedtelematic.libats.http.HttpCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import cats.syntax.either._
import com.advancedtelematic.libats.data.DataType.HashMethod
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf_server.crypto.Sha256Digest
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient.RequestTargetItem
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.SignedRole
import com.advancedtelematic.tuf.reposerver.db.SignedRoleRepositorySupport

import scala.concurrent.Future
import com.advancedtelematic.tuf.reposerver.util.NamespaceSpecOps._
import com.advancedtelematic.tuf.reposerver.util.{ResourceSpec, TufReposerverSpec}
import eu.timepit.refined.api.Refined
import com.advancedtelematic.libtuf.data.ClientDataType.RoleTypeOps

import scala.concurrent.ExecutionContext.Implicits


class RepoResourceSpec extends TufReposerverSpec
  with ResourceSpec with BeforeAndAfterAll with Inspectors with Whenever with PatienceConfiguration with SignedRoleRepositorySupport {

  implicit val ec = Implicits.global

  val repoId = RepoId.generate()

  val testFile = {
    val checksum = Sha256Digest.digest("hi".getBytes)
    RequestTargetItem(Uri("https://ats.com/testfile"), checksum, targetFormat = None, name = None, version = None, hardwareIds = Seq.empty, length = "hi".getBytes.length)
  }

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  override def beforeAll(): Unit = {
    super.beforeAll()
    fakeKeyserverClient.createRoot(repoId).futureValue
  }

  private def etagHeader(repoId: RepoId) =
    `If-Match`(EntityTag(signedRoleRepo.find(repoId, RoleType.TARGETS).futureValue.checksum.hash.value))

  test("POST returns latest signed json") {
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[SignedPayload[Json]]
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

      val signed = responseAs[SignedPayload[Json]].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile01".refineTry[ValidTargetFilename].get).length shouldBe 2
      targetsRole.targets("myfile02".refineTry[ValidTargetFilename].get).length shouldBe 2
    }
  }

  test("POST returns json with valid hashes") {
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[SignedPayload[Json]].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile".refineTry[ValidTargetFilename].get).hashes(HashMethod.SHA256) shouldBe testFile.checksum.hash
    }
  }

  test("POSTing a file adds uri to custom field") {
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

        val signedPayload = responseAs[SignedPayload[Json]]
        signaturesShouldBeValid(repoId, roleType, signedPayload)
      }
    }
  }

  test("GET on timestamp.json returns a valid Timestamp role") {
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.TIMESTAMP, responseAs[SignedPayload[TimestampRole]])
    }
  }

  test("GET on snapshot.json returns a valid Snapshot role") {
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.SNAPSHOT, responseAs[SignedPayload[SnapshotRole]])
    }
  }

  test("GET on targets.json returns a valid Targets role") {
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      header[ETag] shouldBe 'defined
      signaturesShouldBeValid(newRepoId, RoleType.TARGETS, responseAs[SignedPayload[TargetsRole]])
    }
  }

  test("GET on root.json returns a valid Root role") {
    val newRepoId = addTargetToRepo()

    Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, RoleType.ROOT, responseAs[SignedPayload[RootRole]])
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
      signaturesShouldBeValid(newRepoId, RoleType.ROOT, responseAs[SignedPayload[RootRole]])
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

    val newRole = SignedRole.withChecksum(repoId, RoleType.TIMESTAMP, role, role.signed.version, Instant.now.minus(1, ChronoUnit.DAYS))
    signedRoleRepo.update(newRole).futureValue

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

    val newRole = SignedRole.withChecksum(repoId, RoleType.SNAPSHOT, role, role.signed.version, Instant.now.minus(1, ChronoUnit.DAYS))
    signedRoleRepo.update(newRole).futureValue

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

    val newRole = SignedRole.withChecksum(repoId, RoleType.TARGETS, role, role.signed.version, Instant.now.minus(1, ChronoUnit.DAYS))
    signedRoleRepo.update(newRole).futureValue

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

  test("SnapshotRole includes signed jsons lengths") {
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

        val hash = snapshotRole.meta(RoleType.TARGETS.toMetaPath).hashes(targetsCheckSum.method)

        hash shouldBe targetsCheckSum.hash
      }
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

  test("delegates to keyServer to create root") {
    val newRepoId = RepoId.generate()

    withNamespace("myns") { implicit ns =>
      Post(apiUri(s"repo/${newRepoId.show}")).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
        fakeKeyserverClient.fetchRootRole(newRepoId).futureValue.signed shouldBe a[RootRole]
      }
    }
  }

  test("POST on user_create creates a repository for a namespace") {
    withNamespace("myotherns") { implicit ns =>
      Post(apiUri(s"user_repo")).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
        val newRepoId = responseAs[RepoId]
        fakeKeyserverClient.fetchRootRole(newRepoId).futureValue.signed shouldBe a[RootRole]
      }
    }
  }

  test("creating a target on user_creates adds target to user repo") {
    withNamespace("targetsNs") { implicit ns =>
      val newRepoId = Post(apiUri(s"user_repo")).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RepoId]
      }

      Post(apiUri(s"user_repo/targets/myfile"), testFile).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK

        val signedPayload = responseAs[SignedPayload[Json]]
        signaturesShouldBeValid(newRepoId, RoleType.TARGETS, signedPayload)
      }
    }
  }

  test("getting role after adding a target on user repo returns user role") {
    withNamespace("targetns02") { implicit ns =>
      val newRepoId = Post(apiUri("user_repo")).namespaced ~> routes ~> check {
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

  test("fails if repo for user already exists") {
    withNamespace("targetns03") { implicit ns =>
      Post(apiUri("user_repo")).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }

      Post(apiUri("user_repo")).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.Conflict
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

  test("uploading a target from a uri changes targets json") {
    val repoId = addTargetToRepo()

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

  test("accept name/version, hardwareIds, targetFormat, uri") {
    val repoId = addTargetToRepo()
    val targetfileName: TargetFilename = Refined.unsafeApply("target/with/desc")

    Put(apiUri(s"repo/${repoId.show}/targets/${targetfileName.value}?name=somename&version=someversion&hardwareIds=1,2,3&targetFormat=binary"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val custom = responseAs[SignedPayload[TargetsRole]].signed.targets(targetfileName).customParsed[TargetCustom]

      custom.map(_.name) should contain(TargetName("somename"))
      custom.map(_.version) should contain(TargetVersion("someversion"))
      custom.map(_.hardwareIds.map(_.value)) should contain(Seq("1", "2", "3"))
      custom.flatMap(_.targetFormat) should contain(TargetFormat.BINARY)

      custom.flatMap(_.uri).map(_.toString).get should startWith(storageRoot.toString)
    }
  }

  test("on updates, updatedAt in target custom is updated, createdAt is unchanged") {
    val repoId = addTargetToRepo()
    val targetfileName: TargetFilename = Refined.unsafeApply("target/to/update")

    Put(apiUri(s"repo/${repoId.show}/targets/${targetfileName.value}?name=somename&version=someversion"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val now = Instant.now

    Thread.sleep(1000)

    Put(apiUri(s"repo/${repoId.show}/targets/${targetfileName.value}?name=somename&version=someversion"), form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val custom = responseAs[SignedPayload[TargetsRole]].signed.targets(targetfileName).customParsed[TargetCustom]

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

  val offlineTargetFilename: TargetFilename = Refined.unsafeApply("some/file/name")

  val offlineTargets = {
    val targetCustomJson =
      TargetCustom(TargetName("name"), TargetVersion("version"), Seq.empty, TargetFormat.BINARY.some)
        .asJson
        .deepMerge(Json.obj("uri" -> Uri("https://ats.com").asJson))

    val hashes: ClientHashes = Map(HashMethod.SHA256 -> Refined.unsafeApply("8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"))

    Map(offlineTargetFilename -> ClientTargetItem(hashes, 0, targetCustomJson.some))
  }

  def buildSignedTargetsRole(repoId: RepoId, targets: Map[TargetFilename, ClientTargetItem]): SignedPayload[TargetsRole] = {
    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), targets, 2)
    fakeKeyserverClient.sign(repoId, RoleType.TARGETS, targetsRole).futureValue
  }

  test("accepts an offline signed targets.json") {
    val repoId = addTargetToRepo()
    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
      header[ETag].map(_.value()) should contain(etagHeader(repoId).value())
    }

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targets = responseAs[SignedPayload[TargetsRole]].signed.targets
      targets.keys should contain(offlineTargetFilename)
    }
  }

  test("reject putting offline signed targets.json without etag if it exists already") {
    val repoId = addTargetToRepo()
    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.PreconditionRequired
    }
  }

  test("getting offline target item redirects to custom url") {
    val repoId = addTargetToRepo()

    val signedPayload = buildSignedTargetsRole(repoId, offlineTargets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
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

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
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

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
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

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("target item error some/file/name: All offline signed target items must contain custom metadata")
    }
  }

  test("rejects requests with invalid/missing checksums") {
    val clientTargetItem = offlineTargets(offlineTargetFilename).copy(hashes = Map.empty)
    val targets = Map(offlineTargetFilename -> clientTargetItem)
    val signedPayload = buildSignedTargetsRole(repoId, targets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("Invalid/Missing Checksum")
    }
  }

  test("rejects requests with no uri in target custom") {
    val repoId = addTargetToRepo()

    val targetCustomJson = TargetCustom(TargetName("name"), TargetVersion("version"), Seq.empty, TargetFormat.BINARY.some).asJson

    val hashes: ClientHashes = Map(HashMethod.SHA256 -> Refined.unsafeApply("8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"))
    val targets = Map(offlineTargetFilename -> ClientTargetItem(hashes, 0, targetCustomJson.some))
        val signedPayload = buildSignedTargetsRole(repoId, targets)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("List(DownField(uri))")
    }
  }

  test("rejects offline targets.json with bad signatures") {
    val repoId = addTargetToRepo()

    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), Map.empty, 2)

    val invalidSignedPayload = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, "something else signed").futureValue

    val signedPayload = SignedPayload(invalidSignedPayload.signatures, targetsRole)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("Invalid signature for key")
    }
  }

  test("rejects offline targets.json with less signatures than the required threshold") {
    val repoId = addTargetToRepo()

    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), Map.empty, 2)

    val signedPayload = SignedPayload(Seq.empty, targetsRole)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include("Valid signature count must be >= threshold")
    }
  }

  test("rejects offline targets.json if public keys are not available") {
    val repoId = addTargetToRepo()

    val targetFilename: TargetFilename = Refined.unsafeApply("some/file/name")
    val targets = Map(targetFilename -> ClientTargetItem(Map.empty, 0, None))

    val targetsRole = TargetsRole(Instant.now().plus(1, ChronoUnit.DAYS), targets, 2)

    val EdTufKeyPair(pub, sec) = TufCrypto.generateKeyPair(EdKeyType, 256)
    val signature = TufCrypto.signPayload(sec, targetsRole).toClient(pub.id)
    val signedPayload = SignedPayload(List(signature), targetsRole)

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include(s"No public key available for key ${pub.id}")
    }
  }

  test("adding a target public key delegates to keyserver") {
    val repoId = addTargetToRepo()
    val pub = TufCrypto.generateKeyPair(EdKeyType, 256).pubkey

    Put(apiUri(s"repo/${repoId.show}/keys/targets"), pub) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"repo/${repoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val rootRole = responseAs[SignedPayload[RootRole]].signed
      rootRole.roles(RoleType.TARGETS).keyids should contain(pub.id)
      rootRole.keys(pub.id) shouldBe pub
    }
  }

  test("return offline targets.json even if expired") {
    val repoId = addTargetToRepo()

    val expiredTargetsRole = TargetsRole(Instant.now().minus(1, ChronoUnit.DAYS), offlineTargets, 2)
    val signedPayload = fakeKeyserverClient.sign(repoId, RoleType.TARGETS, expiredTargetsRole).futureValue

    Put(apiUri(s"repo/${repoId.show}/targets"), signedPayload).withHeaders(etagHeader(repoId)) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    // delete the key to simulate offline targets.json
    fakeKeyserverClient.deletePrivateKey(repoId, signedPayload.signatures.head.keyid).futureValue

    Get(apiUri(s"repo/${repoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  def signaturesShouldBeValid[T : Encoder](repoId: RepoId, roleType: RoleType, signedPayload: SignedPayload[T]): Assertion = {
    val signature = signedPayload.signatures.head
    val signed = signedPayload.signed

    val isValid = TufCrypto.isValid(signature, fakeKeyserverClient.publicKey(repoId, roleType), signed)
    isValid shouldBe true
  }

  def addTargetToRepo(repoId: RepoId = RepoId.generate()): RepoId = {
    fakeKeyserverClient.createRoot(repoId)

    Post(apiUri(s"repo/${repoId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    repoId
  }
}


// Test for message publishing in separate class
// because MemoryMessageBus maintains queue and supports only single subscriber.
//
class RepoResourceTufTargetSpec extends TufReposerverSpec
    with ResourceSpec with PatienceConfiguration {

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  val testFile = {
    val checksum = Sha256Digest.digest("hi".getBytes)
    RequestTargetItem(Uri.Empty, checksum, targetFormat = None, name = None, version = None, hardwareIds = Seq.empty, length = "hi".getBytes.length)
  }

  test("publishes messages to bus on adding target") {
    val repoId = RepoId.generate()
    val source = memoryMessageBus.subscribe[TufTargetAdded]()

    withNamespace(s"default-${repoId.show}") { implicit ns =>
      Post(apiUri(s"repo/${repoId.show}")).namespaced ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }

      Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
        status shouldBe StatusCodes.OK
      }

      source.runWith(Sink.head).futureValue shouldBe a[TufTargetAdded]
    }
  }
}

class RepoResourceTufTargetStoreSpec extends TufReposerverSpec
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
      Post(apiUri(s"repo/${repoId.show}")).namespaced ~> routes ~> check {
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

object JsonErrors {
  import io.circe.generic.semiauto._
  implicit val jsonErrorsEncoder: Encoder[JsonErrors] = deriveEncoder
  implicit val jsonErrorsDecoder: Decoder[JsonErrors] = deriveDecoder
}

case class JsonErrors(errors: NonEmptyList[String]) {
  def head: String = errors.head
}
