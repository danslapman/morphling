val versions = Map(
  "cats" -> "2.8.0",
  "circe" -> "0.14.2",
  "mouse" -> "1.0.11",
  "scalacheck" -> "1.15.3",
  "scalatest" -> "3.2.11",
  "simulacrum" -> "0.5.4",
  "paradise" -> "2.1.1",
  "bm4" -> "0.3.1",
  "scalatestplus-scalacheck" -> "3.2.11.0",
  "glass" -> "0.1.0"
)

val morphling = (project in file("core"))
  .settings(Settings.common)
  .settings(
    name := "morphling",
    ThisBuild / parallelExecution := false,
    crossScalaVersions += "3.1.2",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % versions("cats"),
      "org.typelevel" %% "cats-free" % versions("cats"),
      "org.typelevel" %% "alleycats-core" % versions("cats"),
      "tf.tofu" %% "glass-core" % versions("glass"),
      "tf.tofu" %% "glass-macro" % versions("glass") % Test,
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test
    ),
    libraryDependencies ++= {
      (CrossVersion.partialVersion(scalaVersion.value): @unchecked) match {
        case Some((2, _)) => Seq(
          "com.chuusai" %% "shapeless" % "2.3.3",
        )
        case Some((3, _)) => Seq()
        case Some((_, _)) => Seq()
      }
    }
  )

val `morphling-scalacheck` = (project in file("scalacheck"))
  .dependsOn(morphling % "test->test;compile->compile")
  .settings(Settings.common)
  .settings(
    name := "morphling-scalacheck",
    ThisBuild / parallelExecution := false,
    crossScalaVersions += "3.1.2",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.typelevel" %% "simulacrum-scalafix-annotations" % versions("simulacrum"),
      "org.scalacheck" %% "scalacheck" % versions("scalacheck")
    ),
    libraryDependencies ++= ( CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full))
      case _ =>
        Seq.empty[ModuleID]
    }),
    addCommandAlias(
      "simulacrum",
      "scalafixEnable;scalafix AddSerializable;scalafix AddImplicitNotFound;scalafix TypeClassSupport;"
    )
  )

val `morphling-circe` = (project in file("circe"))
  .dependsOn(morphling % "test->test;compile->compile", `morphling-scalacheck` % "test->test")
  .settings(Settings.common)
  .settings(
    name := "morphling-circe",
    ThisBuild / parallelExecution := false,
    crossScalaVersions += "3.1.2",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % versions("circe"),
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.typelevel" %% "simulacrum-scalafix-annotations" % versions("simulacrum"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "org.scalacheck" %% "scalacheck"  % versions("scalacheck") % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % versions("scalatestplus-scalacheck") % Test
    ),
    libraryDependencies ++= {
      (CrossVersion.partialVersion(scalaVersion.value): @unchecked) match {
        case Some((2, _)) => Seq(
          "com.ironcorelabs" %% "cats-scalatest" % "3.0.0" % Test
        )
        case Some((_, _)) => Seq()
      }
    },
    libraryDependencies ++= ( CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full),
          compilerPlugin("com.olegpy" %% "better-monadic-for" % versions("bm4"))
        )
      case Some((2, y)) =>
        Seq(
          compilerPlugin("com.olegpy" %% "better-monadic-for" % versions("bm4"))
        )
      case _ =>
        Seq.empty[ModuleID]
    }),
    addCommandAlias(
      "simulacrum",
      "scalafixEnable;scalafix AddSerializable;scalafix AddImplicitNotFound;scalafix TypeClassSupport;"
    )
  )

val `morphling-reactivemongo` = (project in file("reactivemongo"))
  .dependsOn(morphling % "test->test;compile->compile", `morphling-scalacheck` % "test->test")
  .settings(Settings.common)
  .settings(
    name := "morphling-reactivemongo",
    ThisBuild / parallelExecution := false,
    crossScalaVersions += "3.1.2",
    libraryDependencies ++= Seq(
      "org.reactivemongo" %% "reactivemongo-bson-api" % "1.0.3",
      "org.typelevel" %% "simulacrum-scalafix-annotations" % versions("simulacrum"),
      "org.typelevel" %% "mouse" % versions("mouse"),
      "org.scalatest" %% "scalatest" % versions("scalatest") % Test,
      "org.scalacheck" %% "scalacheck"  % versions("scalacheck") % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % versions("scalatestplus-scalacheck") % Test
    ),
    libraryDependencies ++= {
      (CrossVersion.partialVersion(scalaVersion.value): @unchecked) match {
        case Some((2, _)) => Seq(
          "org.reactivemongo" %% "reactivemongo-bson-api" % "1.0.3"
        )
        case Some((3, _)) => Seq(
          "org.reactivemongo" %% "reactivemongo-bson-api" % "1.1.0-RC6"
        )
      }
    },
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full),
          compilerPlugin("com.olegpy" %% "better-monadic-for" % versions("bm4"))
        )
      case Some((2, y)) =>
        Seq(
          compilerPlugin("com.olegpy" %% "better-monadic-for" % versions("bm4"))
        )
      case _ =>
        Seq.empty[ModuleID]
    }),
    addCommandAlias(
      "simulacrum",
      "scalafixEnable;scalafix AddSerializable;scalafix AddImplicitNotFound;scalafix TypeClassSupport;"
    )

  )

val `morphling-typed-schema` = (project in file("typedschema"))
  .dependsOn(morphling % "test->test;compile->compile")
  .settings(Settings.common)
  .settings(
    name := "morphling-typed-schema",
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "ru.tinkoff" %% "typed-schema-swagger" % "0.14.3",
      "org.typelevel" %% "simulacrum-scalafix-annotations" % versions("simulacrum"),
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
    }),
    addCommandAlias(
      "simulacrum",
      "scalafixEnable;scalafix AddSerializable;scalafix AddImplicitNotFound;scalafix TypeClassSupport;"
    )
  )

val root = (project in file("."))
  .dependsOn(morphling, `morphling-circe`, `morphling-scalacheck`, `morphling-reactivemongo`, `morphling-typed-schema`)
  .aggregate(morphling, `morphling-circe`, `morphling-scalacheck`, `morphling-reactivemongo`, `morphling-typed-schema`)
  .settings(Settings.common)
  .settings(
    publish := {},
    publishArtifact := false
  )