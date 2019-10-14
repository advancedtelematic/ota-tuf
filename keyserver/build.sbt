libraryDependencies ++= {
  Seq(
    "org.flywaydb" % "flyway-core" % "6.0.4"
  )
}

mainClass in Compile := Some("com.advancedtelematic.tuf.keyserver.Boot")

Revolver.settings

fork := true
