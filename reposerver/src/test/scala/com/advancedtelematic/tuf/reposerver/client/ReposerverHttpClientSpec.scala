package com.advancedtelematic.tuf.reposerver.client

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.BINARY
import com.advancedtelematic.libtuf_server.crypto.Sha256Digest
import com.advancedtelematic.libtuf_server.reposerver.ReposerverClient
import com.advancedtelematic.libtuf_server.reposerver.ReposerverHttpClient
import com.advancedtelematic.tuf.reposerver.util.{HttpClientSpecSupport, ResourceSpec, TufReposerverSpec}

class ReposerverHttpClientSpec extends TufReposerverSpec
    with ResourceSpec
    with HttpClientSpecSupport {

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

  test("can't add target to nonexistant repo") {
    client.addTarget(Namespace("non-existant-namespace"), "filename", Uri("http://example.com"),
                     Sha256Digest.digest("hi".getBytes), 42, BINARY).failed.futureValue shouldBe ReposerverClient.UserRepoNotFound
  }
}
