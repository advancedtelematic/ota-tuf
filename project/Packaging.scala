
import com.typesafe.sbt.packager.docker._
import sbt.Keys._
import sbt._
import com.typesafe.sbt.SbtNativePackager.Docker
import DockerPlugin.autoImport._
import com.typesafe.sbt.SbtNativePackager.autoImport._
import com.typesafe.sbt.packager.linux.LinuxPlugin.autoImport._

object Packaging {
  def docker(distPackageName: String) = {
    Seq(
      dockerRepository in Docker := Some("advancedtelematic"),

      packageName in Docker := distPackageName,

      dockerUpdateLatest in Docker := true,

      defaultLinuxInstallLocation in Docker := s"/opt/${moduleName.value}",

      dockerBaseImage := "advancedtelematic/alpine-jre:8"
    )
  }
}
