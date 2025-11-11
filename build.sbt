ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "scala-playground"
  )
  .aggregate(
    hello_world,
    swing_hello_world,
    regexp,
    jogl
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

lazy val jogl = (project in file("jogl"))
  .settings(
    name := "jogl",
    libraryDependencies ++= Seq(
      "org.jogamp.jogl" % "jogl-all-main" % "2.6.0",
      "org.jogamp.gluegen" % "gluegen-rt-main" % "2.6.0"
    ),
    run / javaOptions ++= Seq(
      "--enable-native-access=ALL-UNNAMED",
      "--add-exports java.base/java.lang=ALL-UNNAMED",
      "--add-exports java.desktop/sun.awt=ALL-UNNAMED",
      "--add-exports java.desktop/sun.java2d=ALL-UNNAMED"
    )
  )