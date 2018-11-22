package com.advancedtelematic.tuf.cli

import com.advancedtelematic.tuf.cli.DataType.TufServerType

import scala.util.control.NoStackTrace

object Errors {
  case class CliArgumentsException(msg: String) extends Exception(msg) with NoStackTrace
  case class CommandNotSupportedByRepositoryType(repoType: TufServerType, msg: String) extends Exception(msg) with NoStackTrace
}
