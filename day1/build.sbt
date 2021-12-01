import Dependencies._

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.joshholl"
ThisBuild / organizationName := "joshholl"

lazy val root = (project in file("."))
  .settings(
    name := "day1",
    libraryDependencies += scalaTest % Test
  )

