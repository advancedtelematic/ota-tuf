libraryDependencies ++= {
  val slickV = "3.2.0"
  val bouncyCastleV = "1.56"

  Seq(
    "com.typesafe.slick" %% "slick" % slickV,
    "com.typesafe.slick" %% "slick-hikaricp" % slickV,

    "org.bouncycastle" % "bcprov-jdk15on" % bouncyCastleV,
    "org.bouncycastle" % "bcpkix-jdk15on" % bouncyCastleV,
    "net.i2p.crypto" % "eddsa" % "0.2.0"
  )
}
