package com.advancedtelematic.tuf.cli

import java.net.URI

object DataType {
  case class KeyName(value: String) extends AnyVal {
    def publicKeyName: String = value + ".pub"
    def privateKeyName: String = value + ".sec"
  }

  case class RepoName(value: String) extends AnyVal

  case class AuthConfig(server: URI, client_id: String, client_secret: String)

  case class RepoConfig(reposerver: URI, auth: Option[AuthConfig])

  case class AuthPlusToken(value: String) extends AnyVal
}
