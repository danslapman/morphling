val versions = Map(
  "cats" -> "2.0.0-RC2",
  "circe" -> "0.11.1",
  "monocle" -> "2.0.0-RC1",
  "mouse" -> "0.22",
  "scalacheck" -> "1.14.0",
  "scalatest" -> "3.0.8",
  "simulacrum" -> "0.19.0",
  "paradise" -> "2.1.1",
  "bm4" -> "0.3.1"
)

val morphling = (project in file("core"))
  .settings(Settings.common)
  .settings(
    name := "morphling",
    parallelExecution in ThisBuild := false,
    crossScalaVersions := Seq("2.12.8", "2.13.0"),
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
    crossScalaVersions := Seq("2.12.8", "2.13.0"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "mouse" % versions("mouse"),
      "com.github.mpilquist" %% "simulacrum" % versions("simulacrum"),
      "org.scalacheck" %% "scalacheck" % versions("scalacheck")
    ),
    libraryDependencies ++= ( CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full))
      case _ =>
        Seq.empty[ModuleID]
    })
  )

val `morphling-circe` = (project in file("circe"))
  .dependsOn(morphling % "test->test;compile->compile", `morphling-scalacheck` % "test->test")
  .settings(Settings.common)
  .settings(
    name := "morphling-circe",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % versions("circe"),
      "org.typelevel" %% "mouse" % versions("mouse"),
      "com.github.mpilquist" %% "simulacrum" % versions("simulacrum"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "org.scalacheck" %% "scalacheck"  % versions("scalacheck") % Test,
      "com.ironcorelabs" %% "cats-scalatest" % "2.4.0"
    ),
    libraryDependencies ++= ( CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full))
      case _ =>
        Seq.empty[ModuleID]
    }),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % versions("bm4"))
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
    libraryDependencies ++= ( CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full))
      case _ =>
        Seq.empty[ModuleID]
    }),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % versions("bm4"))
  )

val `morphling-typed-schema` = (project in file("typedschema"))
  .dependsOn(morphling % "test->test;compile->compile")
  .settings(Settings.common)
  .settings(
    name := "morphling-typed-schema",
    parallelExecution in ThisBuild := false,
    libraryDependencies ++= Seq(
      "ru.tinkoff" %% "typed-schema-swagger" % "0.11.0-beta6",
      "com.github.mpilquist" %% "simulacrum" % versions("simulacrum"),
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "com.stephenn" %% "scalatest-circe" % "0.0.1" % Test
    ),
    libraryDependencies ++= ( CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full))
      case _ =>
        Seq.empty[ModuleID]
    })
  )

val root = (project in file("."))
  .dependsOn(morphling, `morphling-circe`, `morphling-scalacheck`, `morphling-reactivemongo`, `morphling-typed-schema`)
  .aggregate(morphling, `morphling-circe`, `morphling-scalacheck`, `morphling-reactivemongo`, `morphling-typed-schema`)
  .enablePlugins(CrossPerProjectPlugin)
  .settings(Settings.common)
  .settings(
    publish := {},
    bintrayRelease := {},
    bintrayUnpublish := {}
  )