
import com.typesafe.sbt.SbtGit.GitKeys._
import com.typesafe.sbt.packager.SettingsHelper._
import sbtrelease._
import sbtrelease.ReleaseStateTransformations.{setReleaseVersion => _, _}
import sbt.Keys._
import sbt._
import com.typesafe.sbt.SbtNativePackager.Docker

import sbtrelease.ReleasePlugin.autoImport._

object Release {

  lazy val settings = {

    Seq(
      releaseProcess := Seq(
        checkSnapshotDependencies,
        releaseStepCommand("ivySbt +libtuf/publish"),
        ReleaseStep(releaseStepTask(publish in Docker))
      ),
      releaseIgnoreUntrackedFiles := true
    )
  }
}
