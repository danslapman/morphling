import sbt._
import sbt.Keys._
import scalafix.sbt.ScalafixPlugin.autoImport._

object Settings {
  val common = Seq(
    organization := "com.github.danslapman",
    organizationName := "danslapman",
    organizationHomepage := Some(url("https://github.com/danslapman")),
    version := "2.7.0",
    scalaVersion := "2.13.6",
    crossScalaVersions := Seq("2.12.15", "2.13.6"),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) => Seq(
          "-language:higherKinds,implicitConversions",
          "-Ywarn-unused:imports",
          "-deprecation",
          "-Ypartial-unification",
          "-Xsource:3",
          "-P:kind-projector:underscore-placeholders"
        )
        case Some((2, 13)) => Seq(
          "-language:higherKinds,implicitConversions",
          "-Ywarn-unused:imports",
          "-deprecation",
          "-Ymacro-annotations",
          "-Xsource:3",
          "-P:kind-projector:underscore-placeholders"
        )
        case Some((3, _)) => Seq(
          "-Ykind-projector:underscores",
          "-source:future"
        )
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(
          compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
          compilerPlugin(scalafixSemanticdb)
        )
        case _ =>
          Seq.empty[ModuleID]
      }
    },
    ThisBuild / scalafixDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(
          "com.github.liancheng" %% "organize-imports" % "0.6.0"
        )
        case _ =>
          Seq.empty[ModuleID]
      }
    },
    ThisBuild / scalafixDependencies ++= Seq(
      "org.typelevel" %% "simulacrum-scalafix" % "0.5.4"
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/danslapman/morphling"),
        "scm:git@github.com:danslapman/morphling.git"
      )
    ),
    developers := List(
      Developer(
        id    = "danslapman",
        name  = "Daniil Smirnov",
        email = "danslapman@gmail.com",
        url   = url("https://github.com/danslapman")
      )
    ),
    licenses += ("WTFPL", url("http://www.wtfpl.net")),
    homepage := Some(url("https://github.com/danslapman/morphling")),
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true
  )
}
