
import com.typesafe.sbt.SbtGit.GitKeys._
import com.typesafe.sbt.packager.SettingsHelper._
import sbtrelease._
import sbtrelease.ReleaseStateTransformations.{setReleaseVersion => _, _}
import sbt.Keys._
import sbt._

import sbtrelease.ReleasePlugin.autoImport._

object Release {

  def settings(toPublish: Project*) = {
    val publishSteps = toPublish.map(p => ReleaseStep(releaseStepTask(publish in p), enableCrossBuild = true))

    val prepareSteps: Seq[ReleaseStep] = Seq(
      checkSnapshotDependencies)

    val dockerPublishSteps: Seq[ReleaseStep] = Seq(
      releaseStepCommand("keyserver/docker:publish"),
      releaseStepCommand("reposerver/docker:publish")
    )

    val cliS3Release: Seq[ReleaseStep] = Seq(
      releaseStepCommand("cli/universal:packageZipTarball"),
      releaseStepCommand("cli/s3release")
    )

    val allSteps = prepareSteps ++ dockerPublishSteps ++ cliS3Release ++ publishSteps

    Seq(
      releaseIgnoreUntrackedFiles := true,
      releaseProcess := allSteps,
      releaseCrossBuild := true
    )
  }
}
