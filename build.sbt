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
      "com.github.mpilquist" %% "simulacrum" % "0.15.0",
      "org.scalacheck" %% "scalacheck"  % "1.14.0"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
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
      "com.github.mpilquist" %% "simulacrum" % "0.15.0",
      "org.typelevel" %% "jawn-parser" % "0.14.1" % Test,
      "org.scalatest" %% "scalatest" % "3.0.6" % Test,
      "org.scalacheck" %% "scalacheck"  % "1.14.0" % Test
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )

val `morphling-reactivemongo` = (project in file("reactivemongo"))
  .dependsOn(morphling % "test->test;compile->compile", `morphling-scalacheck` % "test->test")
  .settings(Settings.common)
  .settings(
    name := "morphling-reactivemongo",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "org.reactivemongo" %% "reactivemongo-bson" % "0.16.4",
      "com.github.mpilquist" %% "simulacrum" % "0.15.0",
      "org.typelevel" %% "mouse" % "0.20",
      "org.scalatest" %% "scalatest" % "3.0.6" % Test,
      "org.scalacheck" %% "scalacheck"  % "1.14.0" % Test
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )

val `morphling-typed-schema` = (project in file("typedschema"))
  .dependsOn(morphling % "test->test;compile->compile")
  .settings(Settings.common)
  .settings(
    name := "morphling-typed-schema",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "ru.tinkoff" %% "typed-schema" % "0.10.7.1",
      "com.github.mpilquist" %% "simulacrum" % "0.15.0",
      "org.typelevel" %% "mouse" % "0.20",
      "org.scalatest" %% "scalatest" % "3.0.6" % Test,
      "io.circe" %% "circe-literal" % "0.11.1" % Test,
      "org.typelevel" %% "jawn-parser" % "0.14.1" % Test
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )

val root = (project in file("."))
  .dependsOn(morphling, `morphling-circe`, `morphling-scalacheck`, `morphling-reactivemongo`, `morphling-typed-schema`)
  .aggregate(morphling, `morphling-circe`, `morphling-scalacheck`, `morphling-reactivemongo`, `morphling-typed-schema`)
  .settings(Settings.common)
  .settings(
    publish := {},
    bintrayRelease := {},
    bintrayUnpublish := {}
  )