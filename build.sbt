import CustomSettings._
import java.nio.file.Files.{copy => fileCopy}
import java.nio.file.Paths.{get => createPath}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

def itFilter(name: String): Boolean = name endsWith "IntegrationSpec"

def unitFilter(name: String): Boolean = !itFilter(name)

lazy val ItTest = config("it").extend(Test)

lazy val UnitTest = config("ut").extend(Test)

lazy val commonConfigs = Seq(ItTest, UnitTest)

lazy val commonDeps = libraryDependencies ++= {
  val scalaTestV = "3.0.8"
  lazy val libatsV = libatsVersion.value

  Seq(
    "org.scala-lang.modules" %% "scala-async" % "0.9.6",
    "com.advancedtelematic" %% "libats" % libatsV,
    "org.scalatest" %% "scalatest" % scalaTestV % "test"
  )
}

lazy val serverDependencies = libraryDependencies ++= {
  lazy val akkaV = "2.6.5"
  lazy val akkaHttpV = "10.1.11"
  lazy val libatsV = libatsVersion.value
  lazy val slickV = "3.2.0"
  lazy val catsV = "2.0.0"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.typesafe.akka" %% "akka-stream-testkit" % akkaV % "test",
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-slf4j" % akkaV,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV % "test",
    "com.softwaremill.sttp.client" %% "akka-http-backend" % "2.0.6" % "test",

    "com.advancedtelematic" %% "libats-http" % libatsV,
    "com.advancedtelematic" %% "libats-http-tracing" % libatsV,
    "com.advancedtelematic" %% "libats-messaging" % libatsV,
    "com.advancedtelematic" %% "libats-metrics-akka" % libatsV,
    "com.advancedtelematic" %% "libats-metrics-prometheus" % libatsV,
    "com.advancedtelematic" %% "libats-slick" % libatsV,
    "com.advancedtelematic" %% "libats-logging" % libatsV,
    "com.typesafe.slick" %% "slick" % slickV,
    "com.typesafe.slick" %% "slick-hikaricp" % slickV,
    "org.mariadb.jdbc" % "mariadb-java-client" % "2.4.4",

    "org.typelevel" %% "cats-core" % catsV withSources(),
    "org.typelevel" %% "cats-kernel" % catsV,
    "org.typelevel" %% "cats-macros" % catsV
  )
}

lazy val commonSettings = Seq(
  organization := "com.advancedtelematic",
  scalaVersion := "2.12.10",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Xexperimental", "-Ypartial-unification"),
  scalacOptions in (Compile, console) ~= (_.filterNot(_ == "-Ywarn-unused-import")),
  resolvers += "ATS Releases" at "https://nexus.ota.here.com/content/repositories/releases",
  resolvers += "ATS Snapshots" at "https://nexus.ota.here.com/content/repositories/snapshots",
  resolvers += "version99 Empty loggers" at "https://version99.qos.ch",
  libatsVersion := "0.4.1-10-gc950158",
  licenses += ("MPL-2.0", url("http://mozilla.org/MPL/2.0/")),
  buildInfoOptions += BuildInfoOption.ToMap,
  buildInfoOptions += BuildInfoOption.BuildTime,
  dependencyCheckAssemblyAnalyzerEnabled := Some(false)) ++
  Seq(inConfig(ItTest)(Defaults.testTasks): _*) ++
  Seq(inConfig(UnitTest)(Defaults.testTasks): _*) ++
  (testOptions in UnitTest := Seq(Tests.Filter(unitFilter))) ++
  (testOptions in IntegrationTest := Seq(Tests.Filter(itFilter))) ++
  Versioning.settings ++
  commonDeps

lazy val sonarSettings = Seq(
  sonarProperties ++= Map(
    "sonar.projectName" -> "OTA Connect TUF",
    "sonar.sourceEncoding" -> "UTF-8",
    "sonar.projectKey" -> "ota-connect-tuf",
    "sonar.sources" -> "src/main/scala",
    "sonar.tests" -> "src/test/scala",
    "sonar.host.url" -> "https://sonar7.devtools.in.here.com",
    "sonar.links.issue" -> "https://saeljira.it.here.com/projects/OTA/issues",
    "sonar.links.scm" -> "https://main.gitlab.in.here.com/olp/edge/ota/connect/back-end/ota-tuf",
    "sonar.links.ci" -> "https://main.gitlab.in.here.com/olp/edge/ota/connect/back-end/ota-tuf/pipelines",
    "sonar.language" -> "scala",
    "sonar.projectVersion" -> version.value,
    "sonar.modules" -> "libtuf,libtuf-server,keyserver,reposerver,cli",
    "libtuf.sonar.projectName" -> "OTA Connect Libtuf",
    "libtuf.sonar.java.binaries" -> "./target/scala-*/classes",
    "libtuf.sonar.scala.coverage.reportPaths"->"target/scala-2.12/scoverage-report/scoverage.xml",
    "libtuf-server.sonar.projectName" -> "OTA Connect Libtuf Server",
    "libtuf-server.sonar.java.binaries" -> "./target/scala-*/classes",
    "libtuf-server.sonar.scala.coverage.reportPaths"->"target/scala-2.12/scoverage-report/scoverage.xml",
    "keyserver.sonar.projectName" -> "OTA Connect TUF Keyserver",
    "keyserver.sonar.java.binaries" -> "./target/scala-*/classes",
    "keyserver.sonar.scala.coverage.reportPaths"->"target/scala-2.12/scoverage-report/scoverage.xml",
    "reposerver.sonar.projectName" -> "OTA Connect TUF Repository Server",
    "reposerver.sonar.java.binaries" -> "./target/scala-*/classes",
    "reposerver.sonar.scala.coverage.reportPaths"->"target/scala-2.12/scoverage-report/scoverage.xml",
    "cli.sonar.projectName" -> "OTA Connect TUF CLI (garage-sign)",
    "cli.sonar.java.binaries" -> "./target/scala-*/classes",
    "cli.sonar.scala.coverage.reportPaths"->"target/scala-2.12/scoverage-report/scoverage.xml",
  )
)

lazy val libtuf = (project in file("libtuf"))
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin)
  .configs(commonConfigs:_*)
  .settings(commonSettings)
  .settings(Publish.settings)

lazy val libtuf_server = (project in file("libtuf-server"))
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin)
  .configs(commonConfigs:_*)
  .settings(commonSettings)
  .settings(serverDependencies)
  .settings(Publish.settings)
  .dependsOn(libtuf)

lazy val keyserver = (project in file("keyserver"))
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin, JavaAppPackaging)
  .configs(commonConfigs:_*)
  .settings(commonSettings)
  .settings(Publish.disable)
  .settings(Packaging.docker("tuf-keyserver"))
  .settings(serverDependencies)
  .dependsOn(libtuf)
  .dependsOn(libtuf_server)

lazy val reposerver = (project in file("reposerver"))
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin, JavaAppPackaging)
  .configs(commonConfigs:_*)
  .settings(commonSettings)
  .settings(serverDependencies)
  .settings(Publish.disable)
  .settings(Packaging.docker("tuf-reposerver"))
  .dependsOn(libtuf)
  .dependsOn(libtuf_server)

lazy val cli = (project in file("cli"))
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin, JavaAppPackaging, S3ReleasePlugin)
  .configs(commonConfigs:_*)
  .settings(commonSettings)
  .settings(Publish.disable)
  .settings(
    topLevelDirectory := Some("garage-sign"),
    executableScriptName := "garage-sign",
    mappings in Universal += (file("cli/LICENSE") -> "docs/LICENSE"),
    s3Bucket := "ota-tuf-cli-releases",
    libraryDependencies += "com.typesafe" % "config" % "1.3.4" % Test,
    reinstallGarageSign := {
      val home = sys.env("HOME")
      val bin = sys.env("PATH")
                  .split(":")
                  .filter { p =>
                    val f = new File(p + "/garage-sign")
                    p.startsWith(home) && f.isFile && f.isOwnerExecutable
                  }
                  .head
      val targetDir = (new File(bin)).getParent

      stage.value
      val stagingDir = (stagingDirectory in Universal).value
      val files = (stagingDir ** "*").get
      files.foreach { file =>
        val p = file.getAbsolutePath
        if (file.isFile && p.length > stagingDir.absolutePath.length) {
          val relPath = p.substring(stagingDir.getAbsolutePath.length + 1)
          fileCopy(file.toPath, createPath(s"$targetDir/$relPath"), REPLACE_EXISTING)
        }
      }
      println(s"Done installing to $targetDir.")
    }
  )
  .dependsOn(libtuf)

lazy val ota_tuf = (project in file("."))
  .settings(scalaVersion := "2.12.10")
  .settings(Publish.disable)
  .settings(Release.settings(libtuf, libtuf_server, keyserver, reposerver))
  .aggregate(libtuf_server, libtuf, keyserver, reposerver, cli)
  .settings(sonarSettings)
  .settings(aggregate in sonarScan := false)

lazy val reinstallGarageSign = taskKey[Unit]("Reinstall garage-sign in a dir in the home directory")

