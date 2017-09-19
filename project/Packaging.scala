
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

      dockerCommands := Seq(
        Cmd("FROM", "alpine:3.6"),
        Cmd("RUN", "apk upgrade --update && apk add --update openjdk8-jre bash coreutils"),
        ExecCmd("RUN", "mkdir", "-p", s"/var/log/${moduleName.value}"),
        Cmd("ADD", "opt /opt"),
        Cmd("WORKDIR", s"/opt/${moduleName.value}"),
        ExecCmd("ENTRYPOINT", s"/opt/${moduleName.value}/bin/${moduleName.value}"),
        Cmd("RUN", s"chown -R daemon:daemon /opt/${moduleName.value}"),
        Cmd("RUN", s"chown -R daemon:daemon /var/log/${moduleName.value}"),
        Cmd("USER", "daemon")
      )
    )
  }
}


