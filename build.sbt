ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "1.0"
ThisBuild / organization := "com.nosto"
ThisBuild / organizationName := "Nosto"

val springBootVersion = "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "albums-challenge",
    libraryDependencies ++= Seq(
      "org.springframework.boot" % "spring-boot-starter-web" % springBootVersion,
      "org.springframework.boot" % "spring-boot-starter-cache" % springBootVersion,
      "org.springframework" % "spring-context" % "6.1.3",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.16.1",
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.16.1",
      "org.scalameta" %% "munit" % "1.0.0-M10" % Test,
    ),
  )

Compile / mainClass  := Some("albums.challenge.Application")

