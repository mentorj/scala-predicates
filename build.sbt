ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "predicates",
    idePackagePrefix := Some("com.javaxpert.scala")
  )
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % "test"
