package com.advancedtelematic.tuf.reposerver.db

import java.time.Instant

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libtuf.data.ClientCodecs
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.TargetItem
import com.advancedtelematic.tuf.reposerver.util.{FakeKeyserverClient, TufReposerverSpec}
import eu.timepit.refined.api.Refined
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}
import slick.jdbc.MySQLProfile.api._
import io.circe.parser._
import cats.syntax.either._
import com.advancedtelematic.tuf.reposerver.http.SignedRoleGeneration

import scala.concurrent.Future


class AnyvalCodecMigrationSpec extends TufReposerverSpec with DatabaseSpec with PatienceConfiguration
  with Inspectors with SignedRoleRepositorySupport {

  import Schema._
  import com.advancedtelematic.libats.slick.db.SlickUUIDKey._

  private implicit lazy val system = ActorSystem(this.getClass.getSimpleName)

  private implicit val materializer = ActorMaterializer()

  override implicit val ec = scala.concurrent.ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))

  val migration = new AnyvalCodecMigration(FakeKeyserverClient)

  val checksum = """{"method":"sha256","hash":"f1dd71a40a06265079b34a1ffb7bd2d0917c0e3234bf11a72ecb20020d9b9a9b"}"""
  val filename: TargetFilename = Refined.unsafeApply("somefilename")

  def runMigration(repoId: RepoId, customJson: String): Future[Seq[TargetItem]] = {
    val insertQ = sqlu"insert into target_items values('#${repoId.uuid.toString}', '#${filename.value}', 'http://', '#$checksum', 20, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, '#$customJson')"

    for {
      _ <- db.run(insertQ)
      _ <- migration.run
      items <- db.run(targetItems.filter(_.repoId === repoId).result)
    } yield items
  }

  def rawJson(repoId: RepoId): Future[Seq[String]] = {
    val sql = sql"select custom from target_items where repo_id = '#${repoId.uuid.toString}'".as[String]
    db.run(sql)
  }

  test("converts old json to json supported by new codecs") {
    val repoId = RepoId.generate()
    val instant = Instant.now.minusSeconds(2)
    val custom = """{"name":{"value":"qemux86-64-ota"},"version":{"value":"ffd79847609f2c979deed5d81ec87833bd88f35bb15aa860454442db05d3129c"},"hardwareIds":["qemux86-64-ota"],"targetFormat":null}"""
    val items = runMigration(repoId, custom).futureValue

    items.headOption.map(_.repoId) should contain(repoId)
    items.headOption.flatMap(_.custom.map(_.updatedAt)).get.isAfter(instant) shouldBe true

    forAll(rawJson(repoId).futureValue) { raw =>
      decode(raw)(ClientCodecs.targetCustomDerivedDecoder).valueOr(throw _) shouldBe a[TargetCustom]
    }
  }

  test("preserves db dates when createdAt/updatedAt are provided") {
    val repoId = RepoId.generate()
    val instant = Instant.parse("2017-07-10T13:27:28Z")
    val custom = """{"name":{"value":"qemux86-64-ota"},"version":{"value":"ffd79847609f2c979deed5d81ec87833bd88f35bb15aa860454442db05d3129c"},"hardwareIds":["qemux86-64-ota"],"targetFormat":null, "createdAt": "2017-07-10T13:27:28Z", "updatedAt": "2017-07-10T13:27:28Z"}"""
    val items = runMigration(repoId, custom).futureValue

    items.headOption.map(_.repoId) should contain(repoId)
    items.headOption.flatMap(_.custom.map(_.updatedAt)).get.isAfter(instant) shouldBe true

    forAll(rawJson(repoId).futureValue) { raw =>
      decode(raw)(ClientCodecs.targetCustomDerivedDecoder).valueOr(throw _) shouldBe a[TargetCustom]
    }
  }

  test("regenerate target role if json is outdated") {
    val repoId = RepoId.generate()

    FakeKeyserverClient.createRoot(repoId).futureValue
    val roleSigning = new SignedRoleGeneration(FakeKeyserverClient)

    roleSigning.regenerateSignedRoles(repoId).futureValue

    val custom = """{"name":{"value":"qemux86-64-ota"},"version":{"value":"ffd79847609f2c979deed5d81ec87833bd88f35bb15aa860454442db05d3129c"},"hardwareIds":["qemux86-64-ota"],"targetFormat":null, "createdAt": "2017-07-10T13:27:28Z", "updatedAt": "2017-07-10T13:27:28Z"}"""
    val items = runMigration(repoId, custom).futureValue

    items.headOption.map(_.repoId) should contain(repoId)

    signedRoleRepo.find(repoId, RoleType.TARGETS).futureValue.version shouldBe 2
  }
}
