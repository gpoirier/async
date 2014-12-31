name := "async"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions ++= Seq("-feature", "-deprecation", "-Xlint", "-Xfatal-warnings")
