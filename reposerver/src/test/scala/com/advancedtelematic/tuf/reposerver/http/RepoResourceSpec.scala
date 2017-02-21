package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.{StatusCodes, Uri}
import cats.syntax.show.toShowOps
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.crypt.{RsaKeyPair, Sha256Digest}
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleTypeToMetaPathOp, RootRole, SnapshotRole, TargetsRole, TimestampRole}
import com.advancedtelematic.libtuf.data.TufDataType.{HashMethod, RepoId, RoleType, _}
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.prop.Whenever
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{Assertion, BeforeAndAfterAll, Inspectors}
import util.{ResourceSpec, TufReposerverSpec}
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import com.advancedtelematic.libats.codecs.AkkaCirce._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._

import scala.concurrent.Future

class RepoResourceSpec extends TufReposerverSpec
  with ResourceSpec with BeforeAndAfterAll with Inspectors with Whenever with PatienceConfiguration {

  val repoId = RepoId.generate()

  val testFile = {
    val checksum = Sha256Digest.digest("hi".getBytes)
    RequestTargetItem(Uri.Empty, checksum, "hi".getBytes.length)
  }


  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  override def beforeAll(): Unit = {
    super.beforeAll()
    fakeRoleStore.generateKey(repoId)
  }

  test("POST returns latest signed json") {
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[SignedPayload[Json]]
      signaturesShouldBeValid(repoId, signedPayload)

      val signed = signedPayload.signed
      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile").length shouldBe 2
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
      targetsRole.targets("myfile01").length shouldBe 2
      targetsRole.targets("myfile02").length shouldBe 2
    }
  }

  test("POST returns json with valid hashes") {
    Post(apiUri(s"repo/${repoId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[SignedPayload[Json]].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile").hashes(HashMethod.SHA256) shouldBe testFile.checksum.hash
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

        println(responseAs[SignedPayload[Json]].asJson.spaces2)

        val signedPayload = responseAs[SignedPayload[Json]]
        signaturesShouldBeValid(repoId, signedPayload)
      }
    }
  }

  test("GET on timestamp.json returns a valid Timestamp role") {
    val newRepoId = createRepo()

    Get(apiUri(s"repo/${newRepoId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, responseAs[SignedPayload[TimestampRole]])
    }
  }

  test("GET on snapshot.json returns a valid Snapshot role") {
    val newRepoId = createRepo()

    Get(apiUri(s"repo/${newRepoId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, responseAs[SignedPayload[SnapshotRole]])
    }
  }

  test("GET on targets.json returns a valid Targets role") {
    val newRepoId = createRepo()

    Get(apiUri(s"repo/${newRepoId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, responseAs[SignedPayload[TargetsRole]])
    }
  }

  test("GET on root.json returns a valid Root role") {
    val newRepoId = createRepo()

    Get(apiUri(s"repo/${newRepoId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newRepoId, responseAs[SignedPayload[RootRole]])
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

  test("SnapshotRole includes signed jsons lengths") {
    val newRepoId = createRepo()

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
    val newRepoId = createRepo()

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
    val newRepoId = createRepo()

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
      val targetsRole = responseAs[SignedPayload[TimestampRole]]

      targetsRole.signed.version shouldBe 2
    }
  }

  def signaturesShouldBeValid[T : Encoder](repoId: RepoId, signedPayload: SignedPayload[T]): Assertion = {
    val signature = signedPayload.signatures.head.toSignature
    val signed = signedPayload.signed

    val isValid = RsaKeyPair.isValid(fakeRoleStore.publicKey(repoId), signature, signed.asJson.canonical.getBytes)
    isValid shouldBe true
  }

  def createRepo(): RepoId = {
    val newRepoId = RepoId.generate()
    fakeRoleStore.generateKey(newRepoId)

    Post(apiUri(s"repo/${newRepoId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    newRepoId
  }
}
