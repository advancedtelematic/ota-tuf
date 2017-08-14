def itFilter(name: String): Boolean = name endsWith "IntegrationSpec"

def unitFilter(name: String): Boolean = !itFilter(name)

lazy val ItTest = config("it").extend(Test)

lazy val UnitTest = config("ut").extend(Test)

lazy val commonConfigs = Seq(ItTest, UnitTest)

lazy val commonDeps = libraryDependencies ++= {
  val akkaV = "2.4.17"
  val akkaHttpV = "10.0.3"
  val scalaTestV = "3.0.0"
  val libatsV = "0.0.1-93-g2c1a6db"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-slf4j" % akkaV,

    "org.mariadb.jdbc" % "mariadb-java-client" % "1.4.4",

    "org.scala-lang.modules" %% "scala-async" % "0.9.6",

    "com.advancedtelematic" %% "libats" % libatsV,
    "com.advancedtelematic" %% "libats-slick" % libatsV,
    "com.advancedtelematic" %% "libats-messaging" % libatsV,
    "com.advancedtelematic" %% "libats-messaging-datatype" % libatsV,
    "com.advancedtelematic" %% "libats-metrics-akka" % libatsV changing(),

    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV,
    "org.scalatest"     %% "scalatest" % scalaTestV % "test"
  )
}

lazy val commonSettings = Seq(
  organization := "com.advancedtelematic",
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.11.11", "2.12.2"),
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Ywarn-unused-import"),
  resolvers += "ATS Releases" at "http://nexus.advancedtelematic.com:8081/content/repositories/releases",
  resolvers += "ATS Snapshots" at "http://nexus.advancedtelematic.com:8081/content/repositories/snapshots",
  resolvers += "version99 Empty loggers" at "http://version99.qos.ch",
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

lazy val keyserver = (project in file("keyserver"))
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin, JavaAppPackaging)
  .configs(commonConfigs:_*)
  .settings(commonSettings)
  .settings(Publish.disable)
  .settings(Packaging.docker("tuf-keyserver"))
  .dependsOn(libtuf)

lazy val reposerver = (project in file("reposerver"))
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin, JavaAppPackaging)
  .configs(commonConfigs:_*)
  .settings(commonSettings)
  .settings(Publish.disable)
  .settings(Packaging.docker("tuf-reposerver"))
  .dependsOn(libtuf)

lazy val ota_tuf = (project in file("."))
  .settings(Publish.disable)
  .settings(scalaVersion := "2.11.11")
  .settings(crossScalaVersions := Seq("2.11.11", "2.12.2"))
  .settings(Release.settings(libtuf, keyserver, reposerver))
  .aggregate(libtuf, keyserver, reposerver)

