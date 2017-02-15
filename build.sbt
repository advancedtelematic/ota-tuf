def itFilter(name: String): Boolean = name endsWith "IntegrationSpec"

def unitFilter(name: String): Boolean = !itFilter(name)

lazy val ItTest = config("it").extend(Test)

lazy val UnitTest = config("ut").extend(Test)

lazy val commonConfigs = Seq(ItTest, UnitTest)

lazy val commonSettings = Seq(
  organization := "com.advancedtelematic",
  scalaVersion := "2.11.8",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8"),
  resolvers += "ATS Releases" at "http://nexus.prod01.internal.advancedtelematic.com:8081/content/repositories/releases",
  resolvers += "ATS Snapshots" at "http://nexus.prod01.internal.advancedtelematic.com:8081/content/repositories/snapshots",
  buildInfoOptions += BuildInfoOption.ToMap,
  buildInfoOptions += BuildInfoOption.BuildTime) ++
  Seq(inConfig(ItTest)(Defaults.testTasks): _*) ++
  Seq(inConfig(UnitTest)(Defaults.testTasks): _*) ++
  (testOptions in UnitTest := Seq(Tests.Filter(unitFilter))) ++
  (testOptions in IntegrationTest := Seq(Tests.Filter(itFilter))) ++
  Versioning.settings ++
  Release.settings

lazy val libtuf = (project in file("libtuf"))
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin)
  .configs(commonConfigs:_*)
  .settings(commonSettings)

lazy val keyserver = (project in file("keyserver"))
  .enablePlugins(BuildInfoPlugin, Versioning.Plugin)
  .configs(commonConfigs:_*)
  .settings(commonSettings)
  .dependsOn(libtuf)

// lazy val root = (project in file("."))
//  .aggregate(libtuf, keyserver, rolesign)
