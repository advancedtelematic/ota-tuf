libraryDependencies ++= {
  val akkaV = "2.4.14"
  val akkaHttpV = "10.0.0"
  val scalaTestV = "3.0.0"
  val slickV = "3.1.1"
  val sotaV = "0.2.53"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV,
    "com.typesafe.akka" %% "akka-slf4j" % akkaV,
    "org.scalatest"     %% "scalatest" % scalaTestV % "test",

    "ch.qos.logback" % "logback-classic" % "1.1.3",

    "org.genivi" %% "sota-common" % sotaV,
    "org.genivi" %% "sota-common-db-test" % sotaV % "test",

    "org.bouncycastle" % "bcprov-jdk15on" % "1.56",
    "org.bouncycastle" % "bcpkix-jdk15on" % "1.56",

    "org.scala-lang.modules" %% "scala-async" % "0.9.6",

    "com.typesafe.slick" %% "slick" % slickV,
    "com.typesafe.slick" %% "slick-hikaricp" % slickV,
    "org.mariadb.jdbc" % "mariadb-java-client" % "1.4.4",
    "org.flywaydb" % "flyway-core" % "4.0.3"
  )
}

mainClass in Compile := Some("com.advancedtelematic.ota_tuf.Boot")

flywayUrl := sys.env.getOrElse("DB_URL", "jdbc:mysql://localhost:3306/ota_tuf")

flywayUser := sys.env.getOrElse("DB_USER", "ota_tuf")

flywayPassword := sys.env.getOrElse("DB_PASSWORD", "ota_tuf")

import com.typesafe.sbt.packager.docker._

dockerRepository in Docker := Some("advancedtelematic")

packageName in Docker := packageName.value

dockerUpdateLatest in Docker := true

defaultLinuxInstallLocation in Docker := s"/opt/${moduleName.value}"

dockerCommands := Seq(
  Cmd("FROM", "alpine:3.3"),
  Cmd("RUN", "apk upgrade --update && apk add --update openjdk8-jre bash coreutils"),
  ExecCmd("RUN", "mkdir", "-p", s"/var/log/${moduleName.value}"),
  Cmd("ADD", "opt /opt"),
  Cmd("WORKDIR", s"/opt/${moduleName.value}"),
  ExecCmd("ENTRYPOINT", s"/opt/${moduleName.value}/bin/${moduleName.value}"),
  Cmd("RUN", s"chown -R daemon:daemon /opt/${moduleName.value}"),
  Cmd("RUN", s"chown -R daemon:daemon /var/log/${moduleName.value}"),
  Cmd("USER", "daemon")
)

enablePlugins(JavaAppPackaging)

Revolver.settings
