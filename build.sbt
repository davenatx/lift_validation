import Dependencies._

name := "lift_validation"

organization := "com.dmp"

version := "0.1"

scalaVersion := "2.11.4"

jetty()

scalacOptions ++= Seq("-optimize", "-deprecation", "-feature")

libraryDependencies ++= lift_validationDependencies

git.baseVersion := "0.1"

//versionWithGit

showCurrentGitBranch

scalariformSettings

org.scalastyle.sbt.ScalastylePlugin.Settings