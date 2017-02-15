package com.advancedtelematic.ota_tuf.http

import akka.http.scaladsl.model.StatusCodes
import com.advancedtelematic.util.{LongTest, OtaTufSpec, ResourceSpec}
import io.circe.generic.auto._
import org.genivi.sota.marshalling.CirceMarshallingSupport._
import cats.syntax.show._
import com.advancedtelematic.ota_tuf.daemon.KeyGenerationOp
import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType
import com.advancedtelematic.ota_tuf.data.KeyServerDataType.{Key, KeyGenId, RepoId}
import com.advancedtelematic.ota_tuf.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.db.{KeyGenRequestSupport, KeyRepositorySupport}
import io.circe.Json
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration
import io.circe.syntax._
import com.advancedtelematic.ota_tuf.data.Codecs._

import scala.concurrent.{ExecutionContext, Future}

class RootRoleResourceSpec extends OtaTufSpec
  with ResourceSpec
  with KeyGenRequestSupport
  with KeyRepositorySupport
  with PatienceConfiguration
  with Inspectors
  with LongTest {

  implicit val ec = ExecutionContext.global

  val keyGenerationOp = new KeyGenerationOp(fakeVault)

  test("POST returns Accepted") {
    Post(apiUri(s"root/${RepoId.generate().show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }
  }

  test("POST creates key gen request for all types of roles") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue

    requests.size shouldBe RoleType.ALL.size

    requests.map(_.status) should contain only KeyGenRequestStatus.REQUESTED
  }

  test("POST creates all roles") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue

    requests.size shouldBe RoleType.ALL.size

    requests.map(_.roleType) should contain allElementsOf RoleType.ALL
  }

  test("POST fails if key for role already exists") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Conflict
    }
  }

  test("GET returns NotFound if keys do not exist") {
    val repoId = RepoId.generate()

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("GET returns Locked if keys are not ready") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.Locked
    }
  }

  test("GET returns 200 if keys are ready") {
    val repoId = RepoId.generate()

    generateRootRole(repoId).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      println(responseAs[Json].spaces2)

      val signedPayload = responseAs[SignedPayload[RootRole]]
      val rootRole = signedPayload.signed

      signedPayload.signatures should have size RoleType.ALL.size

      rootRole.keys should have size signedPayload.signatures.size
      rootRole.keys should have size RoleType.ALL.size

      rootRole.roles should have size RoleType.ALL.size
      rootRole.roles.keys should contain allElementsOf RoleType.ALL.map(_.show)
    }
  }


  def generateRootRole(repoId: RepoId): Future[Seq[Key]] = {
    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
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

  test("POST to repoId/roletype signs any payload with existing keys ") {
    val repoId = RepoId.generate()

    generateRootRole(repoId).futureValue

    val toSignPayload = Json.fromFields(List("some" -> Json.fromString("signed stuff")))

    Post(apiUri(s"root/${repoId.show}/targets"), toSignPayload) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[SignedPayload[Json]]

      println(signedPayload.asJson.spaces2)

      signedPayload.signatures shouldNot be(empty)

      val targetKey = keyRepo.repoKeys(repoId, RoleType.TARGETS).futureValue.head

      signedPayload.signed shouldBe toSignPayload

      forAll(signedPayload.signatures) { sig =>
        sig.keyid shouldBe targetKey.id

        val isValidT = RoleSigning.isValid(signedPayload.signed, sig, targetKey.publicKey)
        isValidT shouldBe true
      }
    }
  }

  test("returns 404 if there are no keys for given repo/roletype") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}/targets"), Json.Null) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }
}
