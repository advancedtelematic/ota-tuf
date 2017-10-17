import CustomSettings._

def itFilter(name: String): Boolean = name endsWith "IntegrationSpec"

def unitFilter(name: String): Boolean = !itFilter(name)

lazy val ItTest = config("it").extend(Test)

lazy val UnitTest = config("ut").extend(Test)

lazy val commonConfigs = Seq(ItTest, UnitTest)

lazy val commonDeps = libraryDependencies ++= {
  val scalaTestV = "3.0.0"
  lazy val libatsV = libatsVersion.value

  Seq(
    "org.scala-lang.modules" %% "scala-async" % "0.9.6",
    "com.advancedtelematic" %% "libats" % libatsV,
    "org.scalatest" %% "scalatest" % scalaTestV % "test"
  )
}

lazy val serverDependencies = libraryDependencies ++= {
  lazy val akkaV = "2.4.17"
  lazy val akkaHttpV = "10.0.3"
  lazy val libatsV = libatsVersion.value
  lazy val slickV = "3.2.0"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-slf4j" % akkaV,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV % "test",

    "com.advancedtelematic" %% "libats-http" % libatsV,
    "com.advancedtelematic" %% "libats-messaging" % libatsV,
    "com.advancedtelematic" %% "libats-metrics-akka" % libatsV,
    "com.advancedtelematic" %% "libats-slick" % libatsV,
    "com.typesafe.slick" %% "slick" % slickV,
    "com.typesafe.slick" %% "slick-hikaricp" % slickV,
    "org.mariadb.jdbc" % "mariadb-java-client" % "1.4.4"
  )
}

lazy val commonSettings = Seq(
  organization := "com.advancedtelematic",
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.11.11", "2.12.2"),
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Xexperimental"),
  scalacOptions in (Compile, console) ~= (_.filterNot(_ == "-Ywarn-unused-import")),
  resolvers += "ATS Releases" at "http://nexus.advancedtelematic.com:8081/content/repositories/releases",
  resolvers += "ATS Snapshots" at "http://nexus.advancedtelematic.com:8081/content/repositories/snapshots",
  resolvers += "version99 Empty loggers" at "http://version99.qos.ch",
  libatsVersion := "0.1.0-5-g6b585f0",
  buildInfoOptions += BuildInfoOption.ToMap,
  buildInfoOptions += BuildInfoOption.BuildTime) ++
  Seq(inConfig(ItTest)(Defaults.testTasks): _*) ++
  Seq(inConfig(UnitTest)(Defaults.testTasks): _*) ++
  (testOptions in UnitTest := Seq(Tests.Filter(unitFilter))) ++
  (testOptions in IntegrationTest := Seq(Tests.Filter(itFilter))) ++
  Versioning.settings ++
  commonDeps

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
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin, JavaAppPackaging)
  .configs(commonConfigs:_*)
  .settings(commonSettings)
  .settings(Publish.disable)
  .settings(
    topLevelDirectory := Some("garage-sign"),
    executableScriptName := "garage-sign"
  )
  .dependsOn(libtuf)

lazy val ota_tuf = (project in file("."))
  .settings(Publish.disable)
  .settings(scalaVersion := "2.11.11")
  .settings(crossScalaVersions := Seq("2.11.11", "2.12.2"))
  .settings(Release.settings(libtuf, libtuf_server, keyserver, reposerver))
  .aggregate(libtuf_server, libtuf, keyserver, reposerver, cli)
