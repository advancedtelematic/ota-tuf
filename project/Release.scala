
import com.typesafe.sbt.SbtGit.GitKeys._
import com.typesafe.sbt.packager.SettingsHelper._
import sbtrelease._
import sbtrelease.ReleaseStateTransformations.{setReleaseVersion => _, _}
import sbt.Keys._
import sbt._
import com.typesafe.sbt.SbtNativePackager.Docker

import sbtrelease.ReleasePlugin.autoImport._

object Release {

  def settings(toPublish: Project*) = {

    val publishSteps = toPublish.map(p => ReleaseStep(releaseStepTask(publish in p), enableCrossBuild = true))

    val prepareSteps: Seq[ReleaseStep] = Seq(
      checkSnapshotDependencies)

    val dockerPublishSteps: Seq[ReleaseStep] = Seq(
      releaseStepCommand("keyserver/docker:publish"),
      releaseStepCommand("reposerver/docker:publish")
      release
    )

    val allSteps = prepareSteps ++ releaseCrossBuild ++ dockerPublishSteps ++ publishSteps

    Seq(
      releaseIgnoreUntrackedFiles := true,
      releaseProcess := allSteps,
      releaseCrossBuild := true
    )
  }
}
