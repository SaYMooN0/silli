ThisBuild / scalaVersion := "3.7.1"
ThisBuild / organization := "com.example"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked"
  )
)

lazy val root = (project in file("."))
  .aggregate(siliCore, siliCli)
  .settings(
    name := "sili",
    publish / skip := true,
    Compile / unmanagedSourceDirectories := Nil,
    Test / unmanagedSourceDirectories := Nil
  )

lazy val siliCore = (project in file("sili-core"))
  .settings(commonSettings)
  .settings(
    name := "sili-core",
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.1" % Test
  )

lazy val siliCli = (project in file("sili-cli"))
  .dependsOn(siliCore)
  .settings(commonSettings)
  .settings(
    name := "sili-cli",
    Compile / mainClass := Some("sili.cli.Main")
  )