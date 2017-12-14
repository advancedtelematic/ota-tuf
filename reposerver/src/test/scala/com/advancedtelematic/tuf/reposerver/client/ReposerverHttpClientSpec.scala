package com.advancedtelematic.tuf.reposerver.client

import java.nio.file.Files

import com.advancedtelematic.libats.data.RefinedUtils.RefineTry
import akka.http.scaladsl.model.Uri
import akka.stream.scaladsl.{FileIO, Sink}
import akka.util.ByteString
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, TargetName, TargetVersion, ValidTargetFilename}
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.BINARY
import com.advancedtelematic.libtuf_server.crypto.Sha256Digest
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient
import com.advancedtelematic.libtuf_server.reposerver.ReposerverHttpClient
import com.advancedtelematic.tuf.reposerver.util.{HttpClientSpecSupport, ResourceSpec, TufReposerverSpec}
import org.scalatest.concurrent.{Eventually, PatienceConfiguration}
import org.scalatest.time.{Seconds, Span}

class ReposerverHttpClientSpec extends TufReposerverSpec
  with ResourceSpec
  with HttpClientSpecSupport
  with PatienceConfiguration
  with Eventually {

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))

  val client = new ReposerverHttpClient("http://localhost", testHttpClient)

  test("creates a root") {
    val ns = Namespace("create_root")
    client.createRoot(ns).futureValue shouldBe a[RepoId]
  }

  test("can add target") {
    val ns = Namespace(RepoId.generate.toString)
    client.createRoot(ns).futureValue shouldBe a[RepoId]
    client.addTarget(ns, "filename", Uri("http://example.com"),
                     Sha256Digest.digest("hi".getBytes), 42, BINARY).futureValue shouldBe(())
  }

  test("can add target with content") {
    val ns = Namespace("content-ns")
    val tempFile = Files.createTempFile("reposerver-client", ".txt")
    val text = "some string".getBytes
    Files.write(tempFile, text)

    val repoId = client.createRoot(ns).futureValue
    val content = FileIO.fromPath(tempFile)

    client.addTargetFromContent(ns, "myfilename", None, Sha256Digest.digest("hi".getBytes), text.length, BINARY, content, TargetName("fakename"), TargetVersion("0.0.0")).futureValue shouldBe(())

    val bytes = targetStore.retrieve(repoId, "myfilename".refineTry[ValidTargetFilename].get).flatMap {
      _.entity.dataBytes.runWith(Sink.reduce[ByteString](_ ++ _))
    }.futureValue

    bytes.utf8String shouldBe "some string"
  }

  test("can't add target to nonexistant repo") {
    client.addTarget(Namespace("non-existant-namespace"), "filename", Uri("http://example.com"),
                     Sha256Digest.digest("hi".getBytes), 42, BINARY).failed.futureValue shouldBe ReposerverClient.UserRepoNotFound
  }
}
