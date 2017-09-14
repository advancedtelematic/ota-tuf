package com.advancedtelematic.tuf.reposerver.db

import java.sql.Timestamp
import java.time.Instant

import cats.syntax.option._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.advancedtelematic.libats.test.DatabaseSpec
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, RoleType}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.SignedRole
import com.advancedtelematic.tuf.reposerver.util.TufReposerverSpec
import db.migration.ExpireAtPopulateMigration
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}
import slick.jdbc.MySQLProfile.api._

import scala.concurrent.Future


class ExpireAtPopulateMigrationSpec extends TufReposerverSpec with DatabaseSpec with PatienceConfiguration
  with Inspectors with SignedRoleRepositorySupport {

  private implicit lazy val system = ActorSystem(this.getClass.getSimpleName)

  private implicit val materializer = ActorMaterializer()

  override implicit val ec = scala.concurrent.ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))

  val migration = new ExpireAtPopulateMigration()

  val checksum = """{"method":"sha256","hash":"f1dd71a40a06265079b34a1ffb7bd2d0917c0e3234bf11a72ecb20020d9b9a9b"}"""

  def runMigration(repoId: RepoId, roleType: RoleType, customJson: String, expiresAt: Option[Instant]): Future[SignedRole] = {
    val expires = expiresAt.map(Timestamp.from)
    val insertQ = sqlu"insert into signed_roles values(${repoId.uuid.toString}, ${roleType.toString}, $checksum, $customJson, 20, 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, $expires)"

    for {
      _ <- db.run(insertQ)
      _ <- migration.run
      role <- signedRoleRepo.find(repoId, roleType)
    } yield role
  }

  test("adds expires_at when expires_at is null") {
    val repoId = RepoId.generate()
    val custom = """{"signatures":[{"keyid":"5717a13ce1dfce799a26f6809c61cacedcb18a182ed350612de91e9ca8ab0946","method":"rsassa-pss","sig":"HMKrrZ23hnr5XdLYSou9CFFB1rm00oOuD0d4a5hz3MAPnWO3Y0OGVoCSFgAorTuaZC+3HI7EAasv0yGVVBtcHMRbl9zBgAh/IyMBAHDmqwtg5amFb+sQS4a7h5gkZuQO7zSaezVLFo+krFaeSb6QiDfFP46wakrotK+nhjGtIHHXiZM9pb1oNJ0tfxaSbJItydnITEeB+NStX62E7OgVjbjlBSg3a894geuURtGPPFcwFH+oImXu745jMNg9Sm+MAxO+PLkSdzlprKDmFDCWGXGpjWq6F6gal6uMepuj0VsCTEKn+UceWKH1WvpFHMWExc8xi/nrMhMD3smK1TMrRQ=="}],"signed":{"expires":"2017-10-06T12:11:32Z","targets":{},"version":1,"_type":"Targets"}}"""
    val role = runMigration(repoId, RoleType.TIMESTAMP, custom, None).futureValue

    role.expireAt shouldBe Instant.parse("2017-10-06T12:11:32Z")
  }

  test("does nothing when expires_at is already set") {
    val repoId = RepoId.generate()
    val custom = """{"signatures":[{"keyid":"5717a13ce1dfce799a26f6809c61cacedcb18a182ed350612de91e9ca8ab0946","method":"rsassa-pss","sig":"HMKrrZ23hnr5XdLYSou9CFFB1rm00oOuD0d4a5hz3MAPnWO3Y0OGVoCSFgAorTuaZC+3HI7EAasv0yGVVBtcHMRbl9zBgAh/IyMBAHDmqwtg5amFb+sQS4a7h5gkZuQO7zSaezVLFo+krFaeSb6QiDfFP46wakrotK+nhjGtIHHXiZM9pb1oNJ0tfxaSbJItydnITEeB+NStX62E7OgVjbjlBSg3a894geuURtGPPFcwFH+oImXu745jMNg9Sm+MAxO+PLkSdzlprKDmFDCWGXGpjWq6F6gal6uMepuj0VsCTEKn+UceWKH1WvpFHMWExc8xi/nrMhMD3smK1TMrRQ=="}],"signed":{"expires":"2018-01-01T00:00:00","targets":{},"version":1,"_type":"Targets"}}"""
    val oldExpires = Instant.parse("2018-01-02T00:00:00Z")
    val role = runMigration(repoId, RoleType.TIMESTAMP, custom, oldExpires.some).futureValue

    role.expireAt shouldBe oldExpires
  }
}
