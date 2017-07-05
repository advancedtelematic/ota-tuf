libraryDependencies ++= {
  Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "org.flywaydb" % "flyway-core" % "4.0.3"
  )
}

mainClass in Compile := Some("com.advancedtelematic.tuf.keyserver.Boot")

Revolver.settings

fork := true
