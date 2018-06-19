package com.advancedtelematic.tuf.keyserver

trait VersionInfo {
  lazy val projectName: String = buildinfo.BuildInfo.name

  lazy val version: String = {
    val bi = buildinfo.BuildInfo
    s"${bi.name}/${bi.version}"
  }

  lazy val builtAtMillis: Long = buildinfo.BuildInfo.builtAtMillis

  lazy val versionMap: Map[String, Any] = buildinfo.BuildInfo.toMap
}
