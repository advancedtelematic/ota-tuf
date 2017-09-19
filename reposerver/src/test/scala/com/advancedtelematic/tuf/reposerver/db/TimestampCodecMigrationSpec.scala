package com.advancedtelematic.tuf.reposerver.db


import com.advancedtelematic.libtuf.data.ClientDataType.{RoleTypeToMetaPathOp, SnapshotRole, TimestampRole}
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.syntax.either._
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.SignedRole
import com.advancedtelematic.tuf.reposerver.util.{FakeKeyserverClient, TufReposerverSpec}
import db.migration.TimestampCodecMigration
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.tuf.reposerver.http.SignedRoleGeneration

import scala.concurrent.Future


class TimestampCodecMigrationSpec extends TufReposerverSpec with DatabaseSpec with PatienceConfiguration
  with Inspectors with SignedRoleRepositorySupport {

  private implicit lazy val system = ActorSystem(this.getClass.getSimpleName)

  private implicit val materializer = ActorMaterializer()

  override implicit val ec = scala.concurrent.ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))

  val migration = new TimestampCodecMigration(FakeKeyserverClient)

  val checksum = """{"method":"sha256","hash":"f1dd71a40a06265079b34a1ffb7bd2d0917c0e3234bf11a72ecb20020d9b9a9b"}"""

  val roleSigning = new SignedRoleGeneration(FakeKeyserverClient)

  def runMigration(repoId: RepoId, roleType: RoleType, customJson: String): Future[SignedRole] = {
    val insertQ = sqlu"update signed_roles set content = $customJson where repo_id = ${repoId.uuid.toString} and role_type = ${roleType.toString}"

    for {
      _ <- FakeKeyserverClient.createRoot(repoId)
      _ <- roleSigning.regenerateSignedRoles(repoId)
      _ <- db.run(insertQ)
      _ <- migration.run
      role <- signedRoleRepo.find(repoId, roleType)
    } yield role
  }

  test("converts old timestamps.json json to json supported by new codecs") {
    val repoId = RepoId.generate()

    val custom = """{"signatures":[{"keyid":"d7fa33889a80eeb05f1a7f8fd76d7d52bfdde1bdb83ae05c586a98a4b885cb2e","method":"rsassa-pss","sig":"iSGw2P/0SUc4YuIZEIUJt4QuPR00I13PTKZ0GmpjfV2dkzsctqciT9loBRbsQz4PHv632qZVnWsqoDKP4PkegnwxHXbmMatZiWW5Zxzw97RbMuUUb1vMmzmiwXL77WqEdgtGbgFyI4SE4+yEexJ/WA433xNGl/yAStH2enLiJszT6M5KGJZq0pS0nKDD6cFGpDmLL93Qdd2MtqhxVsOlo0Pb5lNYtDWr/wMXSoAtZEa7omo/7A/JF3kCij7HYsvgWHUICQbe2oIJIV/skRMsEjAhu/aSmCq5hN7eqiaOy05RZvpftRKUwIDXZz9dK6AXWSGdTDfCvKh2Np9bM2eYwQ=="}],"signed":{"meta":{"snapshot.json":{"hashes":{"sha256":"a48422490f9e92d5f9735fcb85bc7468b614309c19599033c95d9ccb834fbada"},"length":787}},"expires":"2017-09-23T11:34:28Z","version":78,"_type":"Timestamp"}}"""
    val item = runMigration(repoId, RoleType.TIMESTAMP, custom).futureValue

    item.content.signed.as[TimestampRole].valueOr(throw _).meta(RoleType.SNAPSHOT.toMetaPath).version shouldBe 1
  }

  test("converts old snapshot.json json to json supported by new codecs") {
    val repoId = RepoId.generate()

    val custom = """{"signatures":[{"keyid":"e6dc1f34bc430744f3bffd805a9aa512891d3a7f14d8c48126316d8616ae51c4","method":"rsassa-pss","sig":"ubMPi5W9IwmCB+Dmol10+VwrWiOQSNDR25vPza/OvDYdf4YF/T4wQ/GXPBmOJOSeNK3imUsoUkOcVcYfQ1tt7X+p0MhtoJdaNZKuG3kQ2jLVacuKUCDEJtv0PNjQIQXutjy6K9mXxMe7aehaZeI0cfBSEhh9P3fjHAVYe3Xfs6x2UMG6rD+Ts7mJhAZhia++L7eqfwlX8F5IMeKyF1yElFtUQfAaalPR90gHy3Qz3b1maWZs9ax8knM8MU6bCyhRdsWDXRzA1VfU/EOrViJ8nQkLKgqGw2Rn89i2PbhkbwjZstNEj+ZoUp174ZQSBZ6/6CYUuW+AREiJFsLdEhCfqw=="}],"signed":{"meta":{"root.json":{"hashes":{"sha256":"c403b4ad203e96ab4add33a7f52a84d88e4479841c87fad521eff89a0f2080cb"},"length":3274},"targets.json":{"hashes":{"sha256":"675953c035cd452a89fb66032496e8d45e7549b6565096d6f73ac01295c8b7b1"},"length":23118}},"expires":"2017-09-23T11:34:28Z","version":78,"_type":"Snapshot"}}"""
    val item = runMigration(repoId, RoleType.SNAPSHOT, custom).futureValue

    item.content.signed.as[SnapshotRole].valueOr(throw _).meta(RoleType.ROOT.toMetaPath).version shouldBe 1
  }


  test("preserves db content when no update is needed") {
    val repoId = RepoId.generate()
    val custom = """{"signatures":[{"keyid":"d7fa33889a80eeb05f1a7f8fd76d7d52bfdde1bdb83ae05c586a98a4b885cb2e","method":"rsassa-pss","sig":"iSGw2P/0SUc4YuIZEIUJt4QuPR00I13PTKZ0GmpjfV2dkzsctqciT9loBRbsQz4PHv632qZVnWsqoDKP4PkegnwxHXbmMatZiWW5Zxzw97RbMuUUb1vMmzmiwXL77WqEdgtGbgFyI4SE4+yEexJ/WA433xNGl/yAStH2enLiJszT6M5KGJZq0pS0nKDD6cFGpDmLL93Qdd2MtqhxVsOlo0Pb5lNYtDWr/wMXSoAtZEa7omo/7A/JF3kCij7HYsvgWHUICQbe2oIJIV/skRMsEjAhu/aSmCq5hN7eqiaOy05RZvpftRKUwIDXZz9dK6AXWSGdTDfCvKh2Np9bM2eYwQ=="}],"signed":{"meta":{"snapshot.json":{"hashes":{"sha256":"a48422490f9e92d5f9735fcb85bc7468b614309c19599033c95d9ccb834fbada"},"length":787, "version": 20}},"expires":"2017-09-23T11:34:28Z","version":78,"_type":"Timestamp"}}"""
    val item = runMigration(repoId, RoleType.TIMESTAMP, custom).futureValue

    item.content.signed.as[TimestampRole].valueOr(throw _).meta(RoleType.SNAPSHOT.toMetaPath).version shouldBe 20
  }
}
