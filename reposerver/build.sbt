libraryDependencies ++= {
  Seq(
    "org.flywaydb" % "flyway-core" % "4.0.3",
    "com.amazonaws" % "aws-java-sdk-s3" % "1.11.338",
    "com.advancedtelematic" %% "libats-auth" % CustomSettings.libatsVersion.value
  )
}

mainClass in Compile := Some("com.advancedtelematic.tuf.reposerver.Boot")

Revolver.settings

fork := true

