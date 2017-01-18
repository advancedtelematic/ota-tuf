package com.advancedtelematic.ota_tuf.http

import akka.http.scaladsl.model.{StatusCodes, Uri}
import com.advancedtelematic.ota_tuf.data.RepoClientDataType.RoleTypeToMetaPathOp
import com.advancedtelematic.ota_tuf.data.DataType.GroupId
import com.advancedtelematic.util.{OtaTufSpec, ResourceSpec}
import cats.syntax.show.toShowOps
import com.advancedtelematic.ota_tuf.crypt.{RsaKeyPair, Sha256Digest}
import RequestTargetItem._
import com.advancedtelematic.ota_tuf.data.ClientDataType.{RootRole, SignedPayload}
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import io.circe.{Encoder, Json}
import CanonicalJson._
import com.advancedtelematic.ota_tuf.data.Codecs._
import com.advancedtelematic.ota_tuf.data.RepoClientDataType.{SnapshotRole, TargetsRole, TimestampRole}
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.HashMethod
import com.advancedtelematic.ota_tuf.data.RoleType
import org.scalatest.{Assertion, BeforeAndAfterAll, Inspectors}
import io.circe.syntax._
import org.scalatest.prop.Whenever

class RepoResourceSpec extends OtaTufSpec
  with ResourceSpec with BeforeAndAfterAll with Inspectors with Whenever {

  val groupId = GroupId.generate()

  val testFile = {
    val checksum = Sha256Digest.digest("hi".getBytes)
    RequestTargetItem(Uri.Empty, checksum, "hi".getBytes.length)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    fakeRoleStore.generateKey(groupId)
  }

  test("POST returns latest signed json") {
    Post(apiUri(s"repo/${groupId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[SignedPayload[Json]]
      signaturesShouldBeValid(groupId, signedPayload)

      val signed = signedPayload.signed
      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile").length shouldBe 2
    }
  }

  test("POST returns json with previous elements") {
    Post(apiUri(s"repo/${groupId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Post(apiUri(s"repo/${groupId.show}/targets/myfile02"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[SignedPayload[Json]].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile01").length shouldBe 2
      targetsRole.targets("myfile02").length shouldBe 2
    }
  }

  test("POST returns json with valid hashes") {
    Post(apiUri(s"repo/${groupId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signed = responseAs[SignedPayload[Json]].signed

      val targetsRole = signed.as[TargetsRole].valueOr(throw _)
      targetsRole.targets("myfile").hashes(HashMethod.SHA256) shouldBe testFile.checksum.hash
    }
  }

  test("fails if there is no root.json available") {
    val unexistingGroupId = GroupId.generate()

    Post(apiUri(s"repo/${unexistingGroupId.show}/targets/otherfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.FailedDependency
    }
  }

  test("GET for each role type returns the signed json with valid signatures") {
    Post(apiUri(s"repo/${groupId.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    forAll(RoleType.ALL.reverse) { roleType =>
      Get(apiUri(s"repo/${groupId.show}/$roleType.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK

        println(responseAs[SignedPayload[Json]].asJson.spaces2)

        val signedPayload = responseAs[SignedPayload[Json]]
        signaturesShouldBeValid(groupId, signedPayload)
      }
    }
  }

  test("GET on timestamp.json returns a valid Timestamp role") {
    val newGroupId = createRepo()

    Get(apiUri(s"repo/${newGroupId.show}/timestamp.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newGroupId, responseAs[SignedPayload[TimestampRole]])
    }
  }

  test("GET on snapshot.json returns a valid Snapshot role") {
    val newGroupId = createRepo()

    Get(apiUri(s"repo/${newGroupId.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newGroupId, responseAs[SignedPayload[SnapshotRole]])
    }
  }

  test("GET on targets.json returns a valid Targets role") {
    val newGroupId = createRepo()

    Get(apiUri(s"repo/${newGroupId.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newGroupId, responseAs[SignedPayload[TargetsRole]])
    }
  }

  test("GET on root.json returns a valid Root role") {
    val newGroupId = createRepo()

    Get(apiUri(s"repo/${newGroupId.show}/root.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      signaturesShouldBeValid(newGroupId, responseAs[SignedPayload[RootRole]])
    }
  }

  test("POST a new target updates snapshot.json") {
    val snapshotRole =
      Get(apiUri(s"repo/${groupId.show}/snapshot.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[SnapshotRole]]
      }

    Post(apiUri(s"repo/${groupId.show}/targets/changesnapshot"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val newTimestampRole =
      Get(apiUri(s"repo/${groupId.show}/snapshot.json"), testFile) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[SnapshotRole]]
      }

    snapshotRole.signed.expires.isBefore(newTimestampRole.signed.expires) shouldBe true

    snapshotRole.signatures.head shouldNot be(newTimestampRole.signatures.head)
  }

  test("POST a new target updates timestamp.json") {
    val timestampRole =
      Get(apiUri(s"repo/${groupId.show}/timestamp.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[TimestampRole]]
      }

    Post(apiUri(s"repo/${groupId.show}/targets/changets"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val newTimestampRole =
      Get(apiUri(s"repo/${groupId.show}/timestamp.json"), testFile) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[TimestampRole]]
      }

    timestampRole.signed.expires.isBefore(newTimestampRole.signed.expires) shouldBe true

    timestampRole.signatures.head shouldNot be(newTimestampRole.signatures.head)
  }

  test("SnapshotRole includes signed jsons lengths") {
    val newGroupid = createRepo()

    val targetsRole =
      Get(apiUri(s"repo/${newGroupid.show}/targets.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[TargetsRole]]
      }

    val rootRole =
      Get(apiUri(s"repo/${newGroupid.show}/root.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[RootRole]]
      }

    Get(apiUri(s"repo/${newGroupid.show}/snapshot.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val signed = responseAs[SignedPayload[SnapshotRole]].signed

      val targetLength = targetsRole.asJson.canonical.length
      signed.meta(RoleType.TARGETS.toMetaPath).length shouldBe targetLength

      val rootLength = rootRole.asJson.canonical.length
      signed.meta(RoleType.ROOT.toMetaPath).length shouldBe rootLength
    }
  }

  test("GET snapshots.json returns json with valid hashes") {
    val newGroupid = createRepo()

    Post(apiUri(s"repo/${newGroupid.show}/targets/myfile"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(apiUri(s"repo/${newGroupid.show}/targets.json")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val targetsRole = responseAs[SignedPayload[TargetsRole]]

      val targetsCheckSum = Sha256Digest.digest(targetsRole.asJson.canonical.getBytes)

      Get(apiUri(s"repo/${newGroupid.show}/snapshot.json")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        val snapshotRole = responseAs[SignedPayload[SnapshotRole]].signed

        val hash = snapshotRole.meta(RoleType.TARGETS.toMetaPath).hashes(targetsCheckSum.method)

        hash shouldBe targetsCheckSum.hash
      }
    }
  }

  def signaturesShouldBeValid[T : Encoder](groupId: GroupId, signedPayload: SignedPayload[T]): Assertion = {
    val signature = signedPayload.signatures.head.toSignature
    val signed = signedPayload.signed

    val isValid = RsaKeyPair.isValid(fakeRoleStore.publicKey(groupId), signature, signed.asJson.canonical.getBytes)
    isValid shouldBe true
  }

  def createRepo(): GroupId = {
    val newGroupId = GroupId.generate()
    fakeRoleStore.generateKey(newGroupId)

    Post(apiUri(s"repo/${newGroupId.show}/targets/myfile01"), testFile) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    newGroupId
  }
}
