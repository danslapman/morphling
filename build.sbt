ThisBuild / scalaVersion := "2.13.14"

publish / skip := true

val versions = Map(
  "cats"                     -> "2.8.0",
  "circe"                    -> "0.14.2",
  "mouse"                    -> "1.0.11",
  "scalacheck"               -> "1.15.3",
  "scalatest"                -> "3.2.11",
  "simulacrum"               -> "1.1.0",
  "paradise"                 -> "2.1.1",
  "bm4"                      -> "0.3.1",
  "scalatestplus-scalacheck" -> "3.2.11.0",
  "glass"                    -> "0.3.0"
)

val scalaVersions = Seq("2.12.20", "2.13.14", "3.3.3")

lazy val morphling = (projectMatrix in file("core"))
  .jvmPlatform(scalaVersions = scalaVersions)
  .settings(Settings.common)
  .settings(
    name := "morphling",
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"      % versions("cats"),
      "org.typelevel" %% "cats-free"      % versions("cats"),
      "org.typelevel" %% "alleycats-core" % versions("cats"),
      "tf.tofu"       %% "glass-core"     % versions("glass"),
      "tf.tofu"       %% "glass-macro"    % versions("glass")     % Test,
      "org.scalatest" %% "scalatest"      % versions("scalatest") % Test
    ),
    libraryDependencies ++= {
      (CrossVersion.partialVersion(scalaVersion.value): @unchecked) match {
        case Some((2, _)) =>
          Seq("com.chuusai" %% "shapeless" % "2.3.3")
        case Some((3, _)) => Seq.empty[ModuleID]
      }
    },
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full))
      case _ => Seq.empty[ModuleID]
    })
  )

lazy val `morphling-scalacheck` = (projectMatrix in file("scalacheck"))
  .dependsOn(morphling % "test->test;compile->compile")
  .jvmPlatform(scalaVersions = scalaVersions)
  .settings(Settings.common)
  .settings(
    name := "morphling-scalacheck",
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "mouse"                           % versions("mouse"),
      "org.scalacheck" %% "scalacheck"                      % versions("scalacheck")
    ),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq("io.github.leviysoft" %% "simulacrum" % versions("simulacrum"))
      case _ => Seq.empty[ModuleID]
    }),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full))
      case _ => Seq.empty[ModuleID]
    })
  )

lazy val `morphling-circe` = (projectMatrix in file("circe"))
  .dependsOn(morphling % "test->test;compile->compile", `morphling-scalacheck` % "test->test")
  .jvmPlatform(scalaVersions = scalaVersions)
  .settings(Settings.common)
  .settings(
    name := "morphling-circe",
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "io.circe"          %% "circe-core"                      % versions("circe"),
      "org.typelevel"     %% "mouse"                           % versions("mouse"),
      "org.scalatest"     %% "scalatest"                       % versions("scalatest")                % Test,
      "org.scalacheck"    %% "scalacheck"                      % versions("scalacheck")               % Test,
      "org.scalatestplus" %% "scalacheck-1-15"                 % versions("scalatestplus-scalacheck") % Test
    ),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq("io.github.leviysoft" %% "simulacrum" % versions("simulacrum"))
      case _ => Seq.empty[ModuleID]
    }),
    libraryDependencies += {
      (CrossVersion.partialVersion(scalaVersion.value): @unchecked) match {
        case Some((2, _)) => "com.ironcorelabs" %% "cats-scalatest" % "3.1.1" % Test
        case Some((3, _)) => "com.ironcorelabs" %% "cats-scalatest" % "4.0.0" % Test
      }
    },
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise"           % versions("paradise") cross CrossVersion.full),
          compilerPlugin("com.olegpy"     %% "better-monadic-for" % versions("bm4"))
        )
      case Some((2, _)) =>
        Seq(
          compilerPlugin("com.olegpy" %% "better-monadic-for" % versions("bm4"))
        )
      case _ =>
        Seq.empty[ModuleID]
    })
  )

lazy val `morphling-reactivemongo` = (projectMatrix in file("reactivemongo"))
  .dependsOn(morphling % "test->test;compile->compile", `morphling-scalacheck` % "test->test")
  .jvmPlatform(scalaVersions = scalaVersions)
  .settings(Settings.common)
  .settings(
    name := "morphling-reactivemongo",
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.reactivemongo" %% "reactivemongo-bson-api"          % "1.0.3",
      "org.typelevel"     %% "mouse"                           % versions("mouse"),
      "org.scalatest"     %% "scalatest"                       % versions("scalatest")                % Test,
      "org.scalacheck"    %% "scalacheck"                      % versions("scalacheck")               % Test,
      "org.scalatestplus" %% "scalacheck-1-15"                 % versions("scalatestplus-scalacheck") % Test
    ),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq("io.github.leviysoft" %% "simulacrum" % versions("simulacrum"))
      case _ => Seq.empty[ModuleID]
    }),
    libraryDependencies ++= {
      (CrossVersion.partialVersion(scalaVersion.value): @unchecked) match {
        case Some((2, _)) =>
          Seq(
            "org.reactivemongo" %% "reactivemongo-bson-api" % "1.0.3"
          )
        case Some((3, _)) =>
          Seq(
            "org.reactivemongo" %% "reactivemongo-bson-api" % "1.1.0-RC6"
          )
      }
    },
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise"           % versions("paradise") cross CrossVersion.full),
          compilerPlugin("com.olegpy"     %% "better-monadic-for" % versions("bm4"))
        )
      case Some((2, _)) =>
        Seq(
          compilerPlugin("com.olegpy" %% "better-monadic-for" % versions("bm4"))
        )
      case _ =>
        Seq.empty[ModuleID]
    })
  )

lazy val `morphling-typed-schema` = (projectMatrix in file("typedschema"))
  .dependsOn(morphling % "test->test;compile->compile")
  .jvmPlatform(scalaVersions = scalaVersions.init)
  .settings(Settings.common)
  .settings(
    name := "morphling-typed-schema",
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "ru.tinkoff"    %% "typed-schema-swagger"            % "0.14.3",
      "org.typelevel" %% "mouse"                           % versions("mouse"),
      "org.scalatest" %% "scalatest"                       % versions("scalatest") % Test,
      "com.stephenn"  %% "scalatest-circe"                 % "0.0.2"               % Test,
      "org.scalaz"    %% "scalaz-core"                     % "7.2.29"              % Test
    ),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq("io.github.leviysoft" %% "simulacrum" % versions("simulacrum"))
      case _ =>
        Seq.empty[ModuleID]
    }),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full))
      case _ =>
        Seq.empty[ModuleID]
    })
  )

lazy val `morphling-tapir` = (projectMatrix in file("tapir"))
  .dependsOn(morphling % "test->test;compile->compile")
  .jvmPlatform(scalaVersions = scalaVersions)
  .settings(Settings.common)
  .settings(
    name := "morphling-tapir",
    ThisBuild / parallelExecution := false,
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-core"                      % "1.0.0",
      "org.typelevel"               %% "mouse"                           % versions("mouse"),
      "org.scalatest"               %% "scalatest"                       % versions("scalatest")                % Test,
      "org.scalacheck"              %% "scalacheck"                      % versions("scalacheck")               % Test,
      "org.scalatestplus"           %% "scalacheck-1-15"                 % versions("scalatestplus-scalacheck") % Test,
      "com.stephenn"                %% "scalatest-circe"                 % "0.2.5"                              % Test,
      "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs"              % "1.0.0"                              % Test,
      "com.softwaremill.sttp.apispec" %% "openapi-circe" % "0.2.1" % Test
    ),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq("io.github.leviysoft" %% "simulacrum" % versions("simulacrum"))
      case _ => Seq.empty[ModuleID]
    }),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y < 13 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % versions("paradise") cross CrossVersion.full))
      case _ =>
        Seq.empty[ModuleID]
    })
  )

lazy val root = (projectMatrix in file("."))
  .aggregate(
    morphling,
    `morphling-circe`,
    `morphling-scalacheck`,
    `morphling-reactivemongo`,
    `morphling-typed-schema`,
    `morphling-tapir`
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .settings(Settings.common)
  .settings(
    crossScalaVersions := Nil,
    publish := {},
    publishArtifact := false,
    publish / skip := true
  )
