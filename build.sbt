val scala3Version = "3.0.0-RC2"

val settings = Seq(
  testFrameworks += new TestFramework("utest.runner.Framework"),
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.8" % "test",
  version := "0.1.0",
  scalaVersion := scala3Version,
  scalacOptions ++= Seq("-Xmax-inlines", "64")
)


lazy val root = project
  .in(file("."))
  .aggregate(`simple-stuff`, `chimney-alike`)
  .settings(settings: _*)

lazy val `simple-stuff` = project
  .in(file("simple-stuff"))
  .settings(settings: _*)
  .settings(
    name := "domain-slices-simple-stuff"
  )

lazy val `chimney-alike` = project
  .in(file("chimney-alike"))
  .settings(settings: _*)
  .settings(
    name := "chimney-alike"
  )