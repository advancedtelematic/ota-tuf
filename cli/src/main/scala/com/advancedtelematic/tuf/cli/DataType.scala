package com.advancedtelematic.tuf.cli

import java.net.URI

import io.circe.Json

object DataType {
  case class KeyName(value: String) extends AnyVal {
    def publicKeyName: String = value + ".pub"
    def privateKeyName: String = value + ".sec"
  }

  case class RepoName(value: String) extends AnyVal

  case class TreehubConfig(oauth2: Option[AuthConfig], no_auth: Boolean, ostree: Json)

  case class AuthConfig(server: URI, client_id: String, client_secret: String)

  sealed trait TufServerType
  case object RepoServer extends TufServerType
  case object Director extends TufServerType

  case class RepoConfig(reposerver: URI, auth: Option[AuthConfig], treehub: TreehubConfig,
                        repoServerType: TufServerType = RepoServer)

  case class AuthPlusToken(value: String) extends AnyVal
}
