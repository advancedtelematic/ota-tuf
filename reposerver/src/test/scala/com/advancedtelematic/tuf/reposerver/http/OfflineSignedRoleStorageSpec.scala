package com.advancedtelematic.tuf.reposerver.http

import java.time.Instant

import cats.syntax.either._
import akka.http.scaladsl.model.Uri
import io.circe.syntax._
import cats.data.Validated.Valid
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, TargetCustom, TargetsRole}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType, TargetFilename, ValidTargetFilename}
import com.advancedtelematic.tuf.reposerver.util.{FakeKeyserverClient, TufReposerverSpec}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import eu.timepit.refined.api.Refined
import io.circe.Json
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}
import com.advancedtelematic.libats.codecs.CirceCodecs._
import com.advancedtelematic.libats.data.DataType.{Checksum, HashMethod, ValidChecksum}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{StorageMethod, TargetItem}
import com.advancedtelematic.tuf.reposerver.db.TargetItemRepositorySupport

class OfflineSignedRoleStorageSpec extends TufReposerverSpec with DatabaseSpec with PatienceConfiguration
  with TargetItemRepositorySupport {

  implicit val ec = scala.concurrent.ExecutionContext.global

  override implicit def patienceConfig: PatienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  val keyserver = FakeKeyserverClient

  val subject = new OfflineSignedRoleStorage(keyserver)

  val signedRoleGeneration = new SignedRoleGeneration(keyserver)

  val defaultCustom = Map(
    "uri" -> "https://example.com".asJson,
    "name" -> "somename".asJson,
    "version" -> "someversion".asJson,
    "hardwareIds" -> Json.arr(),
    "createdAt" -> Instant.now().asJson,
    "updatedAt" -> Instant.now().asJson
  ).asJson

  val mockChecksum = Checksum(HashMethod.SHA256, Refined.unsafeApply[String, ValidChecksum]("5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03"))

  val mockHashes = Map(HashMethod.SHA256 -> mockChecksum.hash)

  val mockFilename = Refined.unsafeApply[String, ValidTargetFilename]("my/filename")

  val mockTargetItems: Map[TargetFilename, ClientTargetItem] = Map(
    mockFilename -> ClientTargetItem(mockHashes, 1, Some(defaultCustom))
  )

  test("allows previously online targets to not contain any custom metadata") {
    val repoId = RepoId.generate()
    keyserver.createRoot(repoId).futureValue

    signedRoleGeneration.addTargetItem(TargetItem(repoId, mockFilename, Uri("https://example.com"), mockChecksum, 22, None, StorageMethod.Managed)).futureValue
    val existingItem = targetItemRepo.findByFilename(repoId, mockFilename).futureValue

    val newTargetItems = Map(mockFilename -> ClientTargetItem(mockHashes, existingItem.length, existingItem.custom.map(_.asJson)))

    val targets = TargetsRole(Instant.now.plusSeconds(3600), newTargetItems, version = 2)
    val payload = keyserver.sign(repoId, RoleType.TARGETS, targets).futureValue

    subject.store(repoId, payload).futureValue shouldBe a[Valid[_]]
  }

  test("keeps previously online targets unchanged when storing new offline targets") {
    val repoId = RepoId.generate()

    keyserver.createRoot(repoId).futureValue

    val oldFilename = Refined.unsafeApply[String, ValidTargetFilename]("my/oldfilename")

    signedRoleGeneration.addTargetItem(TargetItem(repoId, oldFilename, Uri("https://example.com"), mockChecksum, 22, defaultCustom.as[TargetCustom].toOption, StorageMethod.Managed)).futureValue
    val oldTargetItem = targetItemRepo.findByFilename(repoId, oldFilename).futureValue

    val newTargetItems = mockTargetItems + (oldFilename -> ClientTargetItem(mockHashes, oldTargetItem.length, oldTargetItem.custom.map(_.asJson)))
    val targets = TargetsRole(Instant.now.plusSeconds(3600), newTargetItems, version = 2)
    val payload = keyserver.sign(repoId, RoleType.TARGETS, targets).futureValue

    subject.store(repoId, payload).futureValue shouldBe a[Valid[_]]

    targetItemRepo.findByFilename(repoId, oldFilename).futureValue shouldBe oldTargetItem
  }

  test("deletes old target items when storing offline targets") {
    val repoId = RepoId.generate()

    keyserver.createRoot(repoId).futureValue

    val targets = TargetsRole(Instant.now.plusSeconds(3600), mockTargetItems, version = 1)
    val payload = keyserver.sign(repoId, RoleType.TARGETS, targets).futureValue

    subject.store(repoId, payload).futureValue shouldBe a[Valid[_]]
    targetItemRepo.findFor(repoId).futureValue shouldNot be(empty)

    val emptyTargets = TargetsRole(Instant.now.plusSeconds(3600), Map.empty, version = 2)
    val emptyPayload = keyserver.sign(repoId, RoleType.TARGETS, emptyTargets).futureValue

    subject.store(repoId, emptyPayload).futureValue shouldBe a[Valid[_]]
    targetItemRepo.findFor(repoId).futureValue shouldBe empty
  }
}
