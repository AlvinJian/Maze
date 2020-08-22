name := "Maze"

version := "0.3"

scalaVersion := "2.13.2"

scalacOptions += "-deprecation"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-scala" % "4.0.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"

enablePlugins(JavaAppPackaging)
