package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.{StatusCodes, Uri}
import akka.http.scaladsl.server.Route
import com.advancedtelematic.libtuf.crypt.Sha256Digest
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.prop.Whenever
import org.scalatest.{BeforeAndAfterAll, Inspectors}
import cats.syntax.show._
import RequestTargetItem._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._

import scala.concurrent.ExecutionContext
import com.advancedtelematic.tuf.reposerver.util.NamespaceSpecOps._
import com.advancedtelematic.tuf.reposerver.util.{ResourceSpec, TufReposerverSpec}

class RepoResourceNamespaceExtractionSpec extends TufReposerverSpec
  with ResourceSpec with BeforeAndAfterAll with Inspectors with Whenever with PatienceConfiguration {

  implicit val ec = ExecutionContext.global

  override lazy val routes: Route = new RepoResource(fakeRoleStore, NamespaceExtractor.default, localStorage, messageBusPublisher).route

  val testFile = {
    val checksum = Sha256Digest.digest("hi".getBytes)
    RequestTargetItem(Uri.Empty, checksum, "hi".getBytes.length)
  }

  test("reject when repo does not belong to namespace") {
    val repoId = RepoId.generate()

    Post(s"/repo/${repoId.show}/targets/myfile", testFile) ~> Route.seal(routes) ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }

  test("allow when repo belongs to namespace") {
    val repoId = RepoId.generate()

    withNamespace("mynamespace") { implicit ns =>
      Post(s"/repo/${repoId.show}").namespaced ~> Route.seal(routes) ~> check {
        status shouldBe StatusCodes.OK
      }

      Post(s"/repo/${repoId.show}/targets/myfile", testFile).namespaced ~> Route.seal(routes) ~> check {
        status shouldBe StatusCodes.OK
      }
    }
  }

  test("reject when user repo does not belong to user namespace") {
    withNamespace("authnamespace") { implicit ns =>
      Post(s"/user_repo/targets/myfile", testFile).namespaced ~> Route.seal(routes) ~> check {
        status shouldBe StatusCodes.Forbidden
      }
    }
  }

  test("allow when user repo belongs to user namespace") {
    withNamespace("mynamespace02") { implicit ns =>
      Post(s"/user_repo").namespaced ~> Route.seal(routes) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RepoId]
      }

      Post(s"/user_repo/targets/myfile", testFile).namespaced ~> Route.seal(routes) ~> check {
        status shouldBe StatusCodes.OK
      }
    }
  }
}
