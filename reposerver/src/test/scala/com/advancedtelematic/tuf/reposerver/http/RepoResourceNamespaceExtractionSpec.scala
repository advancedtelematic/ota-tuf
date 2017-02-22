package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{StatusCodes, Uri}
import akka.http.scaladsl.server.Route
import com.advancedtelematic.libtuf.crypt.Sha256Digest
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.prop.Whenever
import org.scalatest.{BeforeAndAfterAll, Inspectors}
import util.{ResourceSpec, TufReposerverSpec}
import cats.syntax.show._
import RequestTargetItem._
import de.heikoseeberger.akkahttpcirce.CirceSupport._

import scala.concurrent.ExecutionContext

class RepoResourceNamespaceExtractionSpec extends TufReposerverSpec
  with ResourceSpec with BeforeAndAfterAll with Inspectors with Whenever with PatienceConfiguration {

  implicit val ec = ExecutionContext.global

  override lazy val routes: Route = new RepoResource(fakeRoleStore, NamespaceExtractor.default).route

  val testFile = {
    val checksum = Sha256Digest.digest("hi".getBytes)
    RequestTargetItem(Uri.Empty, checksum, "hi".getBytes.length)
  }

  test("should reject when repo does not belong to namespace") {
    val repoId = RepoId.generate()

    Post(s"/repo/${repoId.show}/targets/myfile", testFile) ~> Route.seal(routes) ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }

  test("should allow when repo belongs to namespace") {
    val repoId = RepoId.generate()

    Post(s"/repo/${repoId.show}").withHeaders(RawHeader("x-ats-namespace", "mynamespace")) ~> Route.seal(routes) ~> check {
      status shouldBe StatusCodes.OK
    }

    Post(s"/repo/${repoId.show}/targets/myfile", testFile).withHeaders(RawHeader("x-ats-namespace", "mynamespace")) ~> Route.seal(routes) ~> check {
      status shouldBe StatusCodes.OK
    }
  }
}
