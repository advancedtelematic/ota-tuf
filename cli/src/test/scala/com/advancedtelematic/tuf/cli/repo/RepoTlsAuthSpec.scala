package com.advancedtelematic.tuf.cli.repo

import java.nio.file.{Files, Paths}

import com.advancedtelematic.libtuf.http.ReposerverClient
import com.advancedtelematic.tuf.cli.DataType.RepoServer
import com.advancedtelematic.tuf.cli.http.TufRepoCliClient
import com.advancedtelematic.tuf.cli.util.{CliSpec, KeyTypeSpecSupport}

class RepoTlsAuthSpec extends CliSpec with KeyTypeSpecSupport {
  import scala.concurrent.ExecutionContext.Implicits.global

  lazy val credentialsZipTlsAuthEd25519 = Paths.get(this.getClass.getResource("/credentials_tls-auth_ed25519.zip").toURI)

  def randomRepoPath = Files.createTempDirectory("tuf-repo").resolve("repo")

  test("can pull targets when authenticated using tls") {
    val repo = RepoManagement.initialize(RepoServer, randomRepoPath, credentialsZipTlsAuthEd25519).get
    val repo2 = new RepoServerRepo(repo.repoPath)

    val client = TufRepoCliClient.forRepo[ReposerverClient](repo2).futureValue

    repo2.repoServerUri.get.toString shouldBe "https://localhost:8181"

    repo2.pullTargets(client).futureValue
  }
}
