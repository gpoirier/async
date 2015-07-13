name := "mapN"

lazy val common = Seq(
  version := "1.0",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-Xlint",
    "-Xfatal-warnings",
    "-language:higherKinds"
  ),
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"
)

lazy val core = project
  .settings(name := "mapN-core")
  .settings(common: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val scalaz = project
  .settings(name := "mapN-scalaz")
  .settings(common: _*)
  .dependsOn(core)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.3"
    )
  )
