libraryDependencies ++= {
  Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "org.flywaydb" % "flyway-core" % "4.0.3",
    "com.amazonaws" % "aws-java-sdk-s3" % "1.11.106"
  )
}

mainClass in Compile := Some("com.advancedtelematic.tuf.reposerver.Boot")

Revolver.settings
