package com.advancedtelematic.tuf.cli

import akka.http.scaladsl.model.Uri

object DataType {
  case class KeyName(value: String) extends AnyVal

  case class RepoName(value: String) extends AnyVal

  case class AuthConfig(server: Uri, client_id: String, client_secret: String)

  case class AuthPlusToken(value: String) extends AnyVal
}
