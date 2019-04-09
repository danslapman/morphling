val versions = Map(
  "cats" -> "1.6.0",
  "circe" -> "0.11.1",
  "monocle" -> "1.5.1-cats",
  "mouse" -> "0.20",
  "scalacheck" -> "1.14.0",
  "scalatest" -> "3.0.6",
  "simulacrum" -> "0.15.0"
)

val morphling = (project in file("core"))
  .settings(Settings.common)
  .settings(
    name := "morphling",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % versions("cats"),
      "org.typelevel" %% "cats-free" % versions("cats"),
      "com.github.julien-truffaut" %%  "monocle-core" % versions("monocle"),
      "com.github.julien-truffaut" %%  "monocle-macro" % versions("monocle") % Test,
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test
    )
  )

val `morphling-scalacheck` = (project in file("scalacheck"))
  .dependsOn(morphling % "test->test;compile->compile")
  .settings(Settings.common)
  .settings(
    name := "morphling-scalacheck",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "mouse" % versions("mouse"),
      "com.github.mpilquist" %% "simulacrum" % versions("simulacrum"),
      "org.scalacheck" %% "scalacheck" % versions("scalacheck")
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
      "io.circe" %% "circe-core" % versions("circe"),
      "org.typelevel" %% "mouse" % "0.20",
      "com.github.mpilquist" %% "simulacrum" % versions("simulacrum"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "org.scalacheck" %% "scalacheck"  % versions("scalacheck") % Test
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")
  )

val `morphling-reactivemongo` = (project in file("reactivemongo"))
  .dependsOn(morphling % "test->test;compile->compile", `morphling-scalacheck` % "test->test")
  .settings(Settings.common)
  .settings(
    name := "morphling-reactivemongo",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "org.reactivemongo" %% "reactivemongo-bson" % "0.16.4",
      "com.github.mpilquist" %% "simulacrum" % versions("simulacrum"),
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "org.scalacheck" %% "scalacheck"  % versions("scalacheck") % Test
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")
  )

val `morphling-typed-schema` = (project in file("typedschema"))
  .dependsOn(morphling % "test->test;compile->compile")
  .settings(Settings.common)
  .settings(
    name := "morphling-typed-schema",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "ru.tinkoff" %% "typed-schema" % "0.10.7.1",
      "com.github.mpilquist" %% "simulacrum" % versions("simulacrum"),
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "com.stephenn" %% "scalatest-circe" % "0.0.1" % Test
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