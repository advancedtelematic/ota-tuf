libraryDependencies ++= {
  Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "org.flywaydb" % "flyway-core" % "4.0.3"
  )
}

mainClass in Compile := Some("com.advancedtelematic.tuf.keyserver.Boot")

flywayUrl := sys.env.getOrElse("DB_URL", "jdbc:mysql://localhost:3306/ota_tuf")

flywayUser := sys.env.getOrElse("DB_USER", "ota_tuf")

flywayPassword := sys.env.getOrElse("DB_PASSWORD", "ota_tuf")

Revolver.settings
