package com.advancedtelematic.ota_tuf.db

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, HashMethod, RoleType, ValidChecksum}
import com.advancedtelematic.ota_tuf.data.KeyServerDataType.RepoId
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.SignedRole
import com.advancedtelematic.util.OtaTufSpec
import io.circe.Json
import org.genivi.sota.core.DatabaseSpec
import org.genivi.sota.http.Errors.RawError
import eu.timepit.refined.refineV
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

import scala.async.Async._
import scala.concurrent.ExecutionContext

class SignedRoleRepositorySpec extends OtaTufSpec with DatabaseSpec with PatienceConfiguration {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))

  test("Fails with Conflict if version cannot be bumped") {
    val repo = new SignedRoleRepository()

    val checksum = Checksum(HashMethod.SHA256, refineV[ValidChecksum]("41b3b0f27a091fe87c3e0f23b4194a8f5f54b1a3c275d0633cb1da1596cc4a6f").right.get)
    val role = SignedRole(RepoId.generate(), RoleType.TARGETS, Json.Null, checksum, 0, 1)

    val ex = async {
      await(repo.persist(role))
      await(repo.persist(role.copy(version = 20)))
    }.failed.futureValue

    ex shouldBe a[RawError]
    ex.asInstanceOf[RawError].responseCode shouldBe StatusCodes.Conflict
    ex.asInstanceOf[RawError].code.code shouldBe "invalid_version_bump"
  }
}
