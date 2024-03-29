ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2023"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"
