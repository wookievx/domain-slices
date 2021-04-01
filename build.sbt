val scala3Version = "3.0.0-RC2"

lazy val `simple-stuff` = project
  .in(file("simple-stuff"))
  .settings(
    name := "domain-slices-simple-stuff",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
