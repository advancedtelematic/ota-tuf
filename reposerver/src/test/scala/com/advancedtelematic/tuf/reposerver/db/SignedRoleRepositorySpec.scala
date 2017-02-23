package com.advancedtelematic.tuf.reposerver.db

import akka.http.scaladsl.model.StatusCodes
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, HashMethod, RoleType, ValidChecksum}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import io.circe.Json
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.SignedRole
import com.advancedtelematic.tuf.reposerver.util.TufReposerverSpec
import eu.timepit.refined.refineV
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

import scala.async.Async._
import scala.concurrent.ExecutionContext

class SignedRoleRepositorySpec extends TufReposerverSpec with DatabaseSpec with PatienceConfiguration {

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
