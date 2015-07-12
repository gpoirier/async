name := "mapN"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-Xlint",
  "-Xfatal-warnings",
  "-language:higherKinds"
)
