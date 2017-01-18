package com.advancedtelematic.ota_tuf.http

import akka.http.scaladsl.model.StatusCodes
import com.advancedtelematic.ota_tuf.data.DataType.{GroupId, Key, KeyGenId}
import com.advancedtelematic.util.{OtaTufSpec, ResourceSpec}
import io.circe.generic.auto._
import org.genivi.sota.marshalling.CirceMarshallingSupport._
import GroupId._
import cats.syntax.show._
import com.advancedtelematic.ota_tuf.daemon.KeyGenerationOp
import com.advancedtelematic.ota_tuf.data.ClientDataType._
import com.advancedtelematic.ota_tuf.data.{KeyGenRequestStatus, RoleType}
import com.advancedtelematic.ota_tuf.db.{KeyGenRequestSupport, KeyRepositorySupport}
import io.circe.Json
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Millis, Seconds, Span}
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}

class RootRoleResourceSpec extends OtaTufSpec
  with ResourceSpec
  with KeyGenRequestSupport
  with KeyRepositorySupport
  with PatienceConfiguration
  with Inspectors {

  implicit val ec = ExecutionContext.global

  val keyGenerationOp = new KeyGenerationOp(fakeVault)

  override implicit def patienceConfig = PatienceConfig(timeout = Span(3, Seconds), interval = Span(100, Millis))

  test("POST returns Accepted") {
    Post(apiUri(s"root/${GroupId.generate().show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }
  }

  test("POST creates key gen request for all types of roles") {
    val groupId = GroupId.generate()

    Post(apiUri(s"root/${groupId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(groupId).futureValue

    requests.size shouldBe RoleType.ALL.size

    requests.map(_.status) should contain only KeyGenRequestStatus.REQUESTED
  }

  test("POST creates all roles") {
    val groupId = GroupId.generate()

    Post(apiUri(s"root/${groupId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(groupId).futureValue

    requests.size shouldBe RoleType.ALL.size

    requests.map(_.roleType) should contain allElementsOf RoleType.ALL
  }

  test("POST fails if key for role already exists") {
    val groupId = GroupId.generate()

    Post(apiUri(s"root/${groupId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    Post(apiUri(s"root/${groupId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Conflict
    }
  }

  test("GET returns NotFound if keys do not exist") {
    val groupId = GroupId.generate()

    Get(apiUri(s"root/${groupId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("GET returns Locked if keys are not ready") {
    val groupId = GroupId.generate()

    Post(apiUri(s"root/${groupId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    Get(apiUri(s"root/${groupId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.Locked
    }
  }

  test("GET returns 200 if keys are ready") {
    val groupId = GroupId.generate()

    generateRootRole(groupId).futureValue

    Get(apiUri(s"root/${groupId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      println(responseAs[Json].spaces2)

      import com.advancedtelematic.ota_tuf.data.Codecs._

      val signedPayload = responseAs[SignedPayload[RootRole]]
      val rootRole = signedPayload.signed

      signedPayload.signatures should have size RoleType.ALL.size

      rootRole.keys should have size signedPayload.signatures.size
      rootRole.keys should have size RoleType.ALL.size

      rootRole.roles should have size RoleType.ALL.size
      rootRole.roles.keys should contain allElementsOf RoleType.ALL.map(_.show)
    }
  }


  def generateRootRole(groupId: GroupId): Future[Seq[Key]] = {
    Post(apiUri(s"root/${groupId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted

      val ids = responseAs[Seq[KeyGenId]]

      Future.sequence {
        ids.map { id =>
          keyGenRepo
            .find(id)
            .flatMap(keyGenerationOp.processGenerationRequest)
        }
      }
    }
  }

  test("POST to groupId/roletype signs any payload with existing keys ") {
    val groupId = GroupId.generate()

    generateRootRole(groupId).futureValue

    val toSignPayload = Json.fromFields(List("some" -> Json.fromString("signed stuff")))

    Post(apiUri(s"root/${groupId.show}/targets"), toSignPayload) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[SignedPayload[Json]]

      println(signedPayload.asJson.spaces2)

      signedPayload.signatures shouldNot be(empty)

      val targetKey = keyRepo.groupKeys(groupId, RoleType.TARGETS).futureValue.head

      signedPayload.signed shouldBe toSignPayload

      forAll(signedPayload.signatures) { sig =>
        sig.keyid shouldBe targetKey.id

        val isValidT = RoleSigning.isValid(signedPayload.signed, sig, targetKey.publicKey)
        isValidT shouldBe true
      }
    }
  }

  test("returns 404 if there are no keys for given group/roletype") {
    val groupId = GroupId.generate()

    Post(apiUri(s"root/${groupId.show}/targets"), Json.Null) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }
}
