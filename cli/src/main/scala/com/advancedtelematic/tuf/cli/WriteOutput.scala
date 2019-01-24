package com.advancedtelematic.tuf.cli

import java.io.{FileOutputStream, OutputStream}

import scala.util.Try

import CliConfigOptionOps._

trait WriteOutput {
  def write(bytes: Array[Byte]): Try[Unit]
}

object WriteOutput {
  def fromConfig(config: Config): WriteOutput = (bytes: Array[Byte]) => Try {
    val outputStream = if (config.inplace)
      new FileOutputStream(config.inputPath.valueOrConfigError.toFile)
    else
      config.outputPath.streamOrStdout

    outputStream.write(bytes)
  }

  def fromOutputStream(out: OutputStream): WriteOutput = (bytes: Array[Byte]) => Try {
    out.write(bytes)
  }
}