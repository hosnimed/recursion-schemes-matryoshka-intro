lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github",
      scalaVersion := "2.12.4"
    )),
    name := "matryoshka-intro"
  )

libraryDependencies ++= Seq(
  "com.slamdata" %% "matryoshka-core" % "0.21.3",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

resolvers += Resolver.sonatypeRepo("snapshots")

scalafmtOnCompile := true