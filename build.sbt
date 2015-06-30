name := "async"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-Xlint",
  "-Xfatal-warnings",
  "-language:higherKinds"
)
