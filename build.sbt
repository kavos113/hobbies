ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "scala-playground"
  )
  .aggregate(
    hello_world,
    swing_hello_world,
    regexp
  )

lazy val hello_world = (project in file("hello-world"))
  .settings(
    name := "hello-world"
  )

lazy val swing_hello_world = (project in file("swing-hello-world"))
  .settings(
    name := "swing-hello-world",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    )
  )

lazy val regexp = (project in file("regexp"))
  .settings(
    name := "regexp"
  )