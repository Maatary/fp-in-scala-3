ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.4.2"
ThisBuild / scalacOptions += ("-explain")
ThisBuild / Compile / run / fork := true

libraryDependencies ++= Seq (
    "org.typelevel" %% "cats-core" % "2.12.0",
    "org.typelevel" %% "cats-effect" % "3.5.4",
    "io.circe" %% "circe-core" % "0.14.7",
    "io.circe" %% "circe-generic" % "0.14.7",
    "io.circe" %% "circe-parser" % "0.14.7",
    "io.circe" %% "circe-optics" % "0.15.0",
    "dev.optics" %% "monocle-core" % "3.2.0",
    "dev.optics" %% "monocle-macro" % "3.2.0"
    )


lazy val root = (project in file("."))
  .settings(
      name := "fp-in-scala-3",
  )
