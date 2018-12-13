libraryDependencies ++= {
  val bouncyCastleV = "1.59"

  Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % bouncyCastleV,
    "org.bouncycastle" % "bcpkix-jdk15on" % bouncyCastleV,
    "net.i2p.crypto" % "eddsa" % "0.3.0-ATS",
    "org.scalaj" %% "scalaj-http" % "2.3.0",
    "org.slf4j" % "slf4j-api" % "1.7.16" % "provided"
  )
}
