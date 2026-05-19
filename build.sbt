ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val copyPascalProgram = taskKey[File]("Copy pascalProgram.txt to target directory")

lazy val root = (project in file("."))
  .settings(
    name := "sili",

    copyPascalProgram := {
      val src = baseDirectory.value / "pascalProgram.txt"
      val dest = (Compile / target).value / "pascalProgram.txt"

      IO.copyFile(src, dest)

      dest
    },

    Compile / compile := (Compile / compile).dependsOn(copyPascalProgram).value
  )