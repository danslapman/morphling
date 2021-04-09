val versions = Map(
  "cats" -> "2.5.0",
  "circe" -> "0.13.0",
  "mouse" -> "0.23",
  "scalacheck" -> "1.15.3",
  "scalatest" -> "3.2.6",
  "simulacrum" -> "1.0.0",
  "paradise" -> "2.1.1",
  "bm4" -> "0.3.1",
  "tofu" -> "0.10.0",
  "scalatestplus-scalacheck" -> "3.2.6.0"
)

val morphling = (project in file("core"))
  .settings(Settings.common)
  .settings(
    name := "morphling",
    ThisBuild / parallelExecution := false,
    crossScalaVersions += "3.0.0-RC2",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(
          "org.typelevel" %% "cats-core" % versions("cats"),
          "org.typelevel" %% "cats-free" % versions("cats"),
          "org.typelevel" %% "alleycats-core" % versions("cats"),
          "tf.tofu" %%  "tofu-optics-core" % versions("tofu"),
          "tf.tofu" %%  "tofu-optics-macro" % versions("tofu") % Test,
          "com.chuusai" %% "shapeless" % "2.3.3",
          "org.scalatest" %% "scalatest" % versions("scalatest") % Test
        )
        case _ => Seq(
          "org.typelevel" %% "cats-core" % versions("cats")
        )
      }
    }
  )

val `morphling-scalacheck` = (project in file("scalacheck"))
  .dependsOn(morphling % "test->test;compile->compile")
  .settings(Settings.common)
  .settings(
    name := "morphling-scalacheck",
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.typelevel" %% "simulacrum" % versions("simulacrum"),
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
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % versions("circe"),
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.typelevel" %% "simulacrum" % versions("simulacrum"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "org.scalacheck" %% "scalacheck"  % versions("scalacheck") % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % versions("scalatestplus-scalacheck") % Test,
      "com.ironcorelabs" %% "cats-scalatest" % "3.0.0"
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
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.reactivemongo" %% "reactivemongo-bson-api" % "1.0.3",
      "org.typelevel" %% "simulacrum" % versions("simulacrum"),
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "org.scalacheck" %% "scalacheck"  % versions("scalacheck") % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % versions("scalatestplus-scalacheck") % Test
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
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "ru.tinkoff" %% "typed-schema-swagger" % "0.14.3",
      "org.typelevel" %% "simulacrum" % versions("simulacrum"),
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "com.stephenn" %% "scalatest-circe" % "0.0.2" % Test,
      "org.scalaz" %% "scalaz-core" % "7.2.29" % Test
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
  .settings(Settings.common)
  .settings(
    publish := {},
    publishArtifact := false
  )