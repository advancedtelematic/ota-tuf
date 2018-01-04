package com.advancedtelematic.tuf.reposerver.http

import java.time.Instant

import io.circe.syntax._
import cats.data.Validated.Valid
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, TargetsRole}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, TargetFilename, ValidTargetFilename}
import com.advancedtelematic.tuf.reposerver.util.{FakeKeyserverClient, TufReposerverSpec}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import eu.timepit.refined.api.Refined
import io.circe.Json
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}
import com.advancedtelematic.libats.codecs.CirceCodecs._
import com.advancedtelematic.libats.data.DataType.{HashMethod, ValidChecksum}
import com.advancedtelematic.tuf.reposerver.db.TargetItemRepositorySupport

class OfflineSignedRoleStorageSpec extends TufReposerverSpec with DatabaseSpec with PatienceConfiguration
  with TargetItemRepositorySupport {

  implicit val ec = scala.concurrent.ExecutionContext.global

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  val keyserver = FakeKeyserverClient

  val subject = new OfflineSignedRoleStorage(keyserver)

  test("deletes old target items when storing offline targets") {
    val repoId = RepoId.generate()

    keyserver.createRoot(repoId).futureValue

    val custom = Map(
      "uri" -> "https://example.com".asJson,
      "name" -> "somename".asJson,
      "version" -> "someversion".asJson,
      "hardwareIds" -> Json.arr(),
      "createdAt" -> Instant.now().asJson,
      "updatedAt" -> Instant.now().asJson
    ).asJson

    val hashes = Map(
      HashMethod.SHA256 -> Refined.unsafeApply[String, ValidChecksum]("5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03")
    )

    val oldItems: Map[TargetFilename, ClientTargetItem] = Map(
      Refined.unsafeApply[String, ValidTargetFilename]("my/filename") -> ClientTargetItem(hashes, 1, Some(custom))
    )

    val targets = TargetsRole(Instant.now.plusSeconds(3600), oldItems, version = 1)
    val payload = keyserver.sign(repoId, RoleType.TARGETS, targets).futureValue

    subject.store(repoId, payload).futureValue shouldBe a[Valid[_]]
    targetItemRepo.findFor(repoId).futureValue shouldNot be(empty)

    val emptyTargets = TargetsRole(Instant.now.plusSeconds(3600), Map.empty, version = 2)
    val emptyPayload = keyserver.sign(repoId, RoleType.TARGETS, emptyTargets).futureValue

    subject.store(repoId, emptyPayload).futureValue shouldBe a[Valid[_]]
    targetItemRepo.findFor(repoId).futureValue shouldBe empty
  }
}
