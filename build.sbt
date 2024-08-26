ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.4.2"
ThisBuild / scalacOptions += ("-explain")
ThisBuild / Compile / run / fork := true

scalaVersion := "3.4.2"

scalacOptions ++= Seq(
  "-new-syntax"
)

libraryDependencies ++= Seq (
    "org.typelevel" %% "cats-core" % "2.12.0",
    "org.typelevel" %% "cats-effect" % "3.5.4",
    "io.circe" %% "circe-core" % "0.14.7",
    "io.circe" %% "circe-generic" % "0.14.7",
    //"io.circe" %% "circe-generic-extras" % "0.14.7",
    "io.circe" %% "circe-parser" % "0.14.9",
    "io.circe" %% "circe-optics" % "0.15.0",
    "dev.optics" %% "monocle-core" % "3.2.0",
    "dev.optics" %% "monocle-macro" % "3.2.0",
    "com.github.fd4s" %% "fs2-kafka" % "3.5.1",
    "co.fs2" %% "fs2-core" % "3.10.2",
    "co.fs2" %% "fs2-io" % "3.10.2",
    "org.gnieh" %% "fs2-data-csv" % "1.11.1",
    "org.gnieh" %% "fs2-data-csv-generic" % "1.11.0",
    "org.typelevel" %% "shapeless3-deriving" % "3.4.0",
    "org.typelevel" %% "kittens" % "3.3.0",
    "io.github.iltotore" %% "iron" % "2.6.0",
    "io.github.iltotore" %% "iron-circe" % "2.6.0",

    )


lazy val root = (project in file("."))
  .settings(
      name := "fp-in-scala-3",
  )
