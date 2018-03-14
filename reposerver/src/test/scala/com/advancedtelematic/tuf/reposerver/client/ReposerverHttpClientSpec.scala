package com.advancedtelematic.tuf.reposerver.client

import java.nio.file.Files

import com.advancedtelematic.libats.data.RefinedUtils.RefineTry
import akka.http.scaladsl.model.Uri
import akka.stream.scaladsl.{FileIO, Sink}
import akka.util.ByteString
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, RepoId, RsaKeyType, TargetName, TargetVersion, ValidTargetFilename}
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.BINARY
import com.advancedtelematic.libtuf_server.crypto.Sha256Digest
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient
import com.advancedtelematic.libtuf_server.reposerver.ReposerverHttpClient
import com.advancedtelematic.tuf.reposerver.util._
import org.scalatest.concurrent.{Eventually, PatienceConfiguration}
import org.scalatest.time.{Seconds, Span}

trait ReposerverHttpClientSpec extends TufReposerverSpec
  with ResourceSpec
  with HttpClientSpecSupport
  with PatienceConfiguration
  with Eventually {

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(30, Seconds))

  val client = new ReposerverHttpClient("http://localhost", testHttpClient)

  test("creates a root") {
    val ns = Namespace("create_root")
    client.createRoot(ns).futureValue shouldBe a[RepoId]
  }

  test("fetches a root") {
    val ns = Namespace("other_root")
    client.createRoot(ns).futureValue
    client.fetchRoot(ns).futureValue.signed shouldBe a[RootRole]
  }

  test("fails if role not on keyserver") {
    val ns = Namespace("no_keyserver_root_other_root")
    val repoId = client.createRoot(ns).futureValue
    fakeKeyserverClient.deleteRepo(repoId)
    client.fetchRoot(ns).failed.futureValue shouldBe ReposerverClient.RootNotInKeyserver
  }

  test("fails if keys not ready") {
    val ns = Namespace("keyserver_keys_not_ready")
    val repoId = client.createRoot(ns).futureValue
    fakeKeyserverClient.forceKeyGenerationPending(repoId)
    client.fetchRoot(ns).failed.futureValue shouldBe ReposerverClient.KeysNotReady
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

  test("can't add target if keys are not in keyserver") {
    val ns = Namespace("creating-repo")
    val repoId = client.createRoot(ns).futureValue
    fakeKeyserverClient.forceKeyGenerationPending(repoId)

    client.addTarget(ns, "filename", Uri("http://example.com"),
      Sha256Digest.digest("hi".getBytes), 42, BINARY).failed.futureValue shouldBe ReposerverClient.KeysNotReady
  }

  test("can't add target to nonexistant repo") {
    client.addTarget(Namespace("non-existant-namespace"), "filename", Uri("http://example.com"),
                     Sha256Digest.digest("hi".getBytes), 42, BINARY).failed.futureValue shouldBe ReposerverClient.NotFound
  }
}

class RsaReposerverHttpClientSpec extends ReposerverHttpClientSpec {
  override val fakeKeyserverClient: FakeKeyserverClient = new FakeKeyserverClient(RsaKeyType)
}

class EdReposerverHttpClientSpec extends ReposerverHttpClientSpec {
  override val fakeKeyserverClient: FakeKeyserverClient = new FakeKeyserverClient(Ed25519KeyType)
}
