libraryDependencies ++= {
  val bouncyCastleV = "1.57"

  Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % bouncyCastleV,
    "org.bouncycastle" % "bcpkix-jdk15on" % bouncyCastleV,
    "org.scalaj" %% "scalaj-http" % "2.3.0"
  )
}
