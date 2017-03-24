package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model.{HttpEntity, Multipart, StatusCodes}
import akka.util.ByteString
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.reposerver.target_store.S3TargetStore
import com.advancedtelematic.tuf.reposerver.util.{ResourceSpec, TufReposerverSpec}
import org.scalatest.{BeforeAndAfterAll, Inspectors}
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.prop.Whenever
import RepoId._
import cats.syntax.show.toShowOps
import com.advancedtelematic.tuf.reposerver.Settings

class S3StorageResourceIntegrationSpec extends TufReposerverSpec
    with ResourceSpec with BeforeAndAfterAll with Inspectors with Whenever with PatienceConfiguration {

  val credentials = new Settings {}.s3Credentials

  val s3Storage = new S3TargetStore(credentials)

  override lazy val routes = new RepoResource(fakeRoleStore, namespaceValidation,
    s3Storage, messageBus).route

  test("uploading a target changes targets json") {
    val repoId = RepoId.generate()
    fakeRoleStore.generateKey(repoId)

    val entity = HttpEntity(ByteString("""
                                         |Like all the men of the Library, in my younger days I traveled;
                                         |I have journeyed in quest of a book, perhaps the catalog of catalogs.
                                       """.stripMargin))

    val fileBodyPart = BodyPart("file", entity, Map("filename" -> "babel.txt"))

    val form = Multipart.FormData(fileBodyPart)

    Put(s"/repo/${repoId.show}/targets/some/target/funky/thing?name=pkgname&version=pkgversion&desc=wat", form) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    Get(s"/repo/${repoId.show}/targets/some/target/funky/thing") ~> routes ~> check {
      status shouldBe StatusCodes.Found
      header("Location").get.value() should include("amazonaws.com")
    }
  }
}
