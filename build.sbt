val versions = Map(
  "cats" -> "1.6.0"
)

val morphling = (project in file("core"))
  .settings(Settings.common)
  .settings(
    name := "morphling",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % versions("cats"),
      "org.typelevel" %% "cats-free" % versions("cats"),
      "com.github.julien-truffaut" %%  "monocle-core" % "1.5.1-cats",
      "com.github.julien-truffaut" %%  "monocle-macro" % "1.5.1-cats" % Test,
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalatest" %% "scalatest" % "3.0.6" % Test
    )
  )

val `morphling-scalacheck` = (project in file("scalacheck"))
  .dependsOn(morphling % "test->test;compile->compile")
  .settings(Settings.common)
  .settings(
    name := "morphling-scalacheck",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "mouse" % "0.20",
      "org.scalacheck" %% "scalacheck"  % "1.14.0"
    )
  )

val `morphling-circe` = (project in file("circe"))
  .dependsOn(morphling % "test->test;compile->compile", `morphling-scalacheck` % "test->test")
  .settings(Settings.common)
  .settings(
    name := "morphling-circe",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.11.1",
      "io.circe" %% "circe-literal" % "0.11.1" % Test,
      "org.typelevel" %% "mouse" % "0.20",
      "org.typelevel" %% "jawn-parser" % "0.14.1" % Test,
      "org.scalatest" %% "scalatest" % "3.0.6" % Test,
      "org.scalacheck" %% "scalacheck"  % "1.14.0" % Test
    )
  )

val root = (project in file("."))
  .dependsOn(morphling, `morphling-circe`, `morphling-scalacheck`)
  .aggregate(morphling, `morphling-circe`, `morphling-scalacheck`)
  .settings(Settings.common)
  .settings(
    publish := {},
    bintrayRelease := {},
    bintrayUnpublish := {}
  )