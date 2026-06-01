ThisBuild / scalaVersion := "3.8.3"

lazy val copyPascalProgram = taskKey[File]("Copy pascalProgram.txt to target directory")

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
    Compile / mainClass := Some("sili.cli.Main"),

    copyPascalProgram := {
      val rootDir = (LocalRootProject / baseDirectory).value

      val src = rootDir / "pascalProgram"
      val dest = (Compile / target).value / "pascalProgram"

      IO.copyFile(src, dest)

      streams.value.log.info(s"Copied $src to $dest")

      dest
    },

    Compile / compile := (Compile / compile)
      .dependsOn(copyPascalProgram)
      .value
  )

addCommandAlias("runCli", "siliCli / run")
addCommandAlias("bgRunCli", "siliCli / Compile / bgRun")