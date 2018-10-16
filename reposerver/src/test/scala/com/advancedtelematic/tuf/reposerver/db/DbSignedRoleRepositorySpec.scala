package com.advancedtelematic.tuf.reposerver.db

import java.time.Instant

import akka.http.scaladsl.model.StatusCodes
import com.advancedtelematic.libats.data.DataType.{Checksum, HashMethod, ValidChecksum}
import com.advancedtelematic.libtuf.data.TufDataType.{JsonSignedPayload, RepoId, RoleType}
import io.circe.Json
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.tuf.reposerver.db.DBDataType.DbSignedRole
import com.advancedtelematic.tuf.reposerver.util.TufReposerverSpec
import eu.timepit.refined.refineV
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

import scala.async.Async._
import scala.concurrent.ExecutionContext

class DbSignedRoleRepositorySpec extends TufReposerverSpec with DatabaseSpec with PatienceConfiguration {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))

  test("Fails with Conflict if version cannot be bumped") {
    val repo = new DbSignedRoleRepository()

    val checksum = Checksum(HashMethod.SHA256, refineV[ValidChecksum]("41b3b0f27a091fe87c3e0f23b4194a8f5f54b1a3c275d0633cb1da1596cc4a6f").right.get)
    val role = DbSignedRole(RepoId.generate(), RoleType.TARGETS, JsonSignedPayload(Seq.empty, Json.Null), checksum, 0, 1, Instant.now)

    val ex = async {
      await(repo.persist(role))
      await(repo.persist(role.copy(version = 20)))
    }.failed.futureValue

    ex shouldBe a[RawError]
    ex.asInstanceOf[RawError].responseCode shouldBe StatusCodes.Conflict
    ex.asInstanceOf[RawError].code.code shouldBe "invalid_version_bump"
  }
}
