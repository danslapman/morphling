import scalafix.sbt.ScalafixPlugin.autoImport._

import sbt.Keys._
import sbt._
import sbtprojectmatrix.ProjectMatrixKeys._

object Settings {
  val common = Seq(
    organization := "com.github.danslapman",
    organizationName := "danslapman",
    organizationHomepage := Some(url("https://github.com/danslapman")),
    scalacOptions ++= {
      (CrossVersion.partialVersion(scalaVersion.value): @unchecked) match {
        case Some((2, 12)) =>
          Seq(
            "-Ywarn-unused-import",
            "-language:higherKinds,implicitConversions",
            "-deprecation",
            "-Ypartial-unification",
            "-Xsource:3",
            "-P:kind-projector:underscore-placeholders"
          )
        case Some((2, 13)) =>
          Seq(
            "-Wunused:imports",
            "-language:higherKinds,implicitConversions",
            "-deprecation",
            "-Ymacro-annotations",
            "-Xsource:3",
            "-P:kind-projector:underscore-placeholders"
          )
        case Some((3, _)) =>
          Seq(
            "-Ykind-projector:underscores",
            "-source:future"
          )
      }
    },
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixConfig := {
      (CrossVersion.partialVersion(scalaVersion.value): @unchecked) match {
        case Some((2, _)) => scalafixConfig.value
        case Some((3, _)) => Some(projectMatrixBaseDirectory.value.getParentFile / ".scalafix3.conf")
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) =>
          Seq(
            compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full),
            compilerPlugin(scalafixSemanticdb)
          )
        case _ =>
          Seq.empty[ModuleID]
      }
    },
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/danslapman/morphling"),
        "scm:git@github.com:danslapman/morphling.git"
      )
    ),
    developers := List(
      Developer(
        id = "danslapman",
        name = "Daniil Smirnov",
        email = "danslapman@gmail.com",
        url = url("https://github.com/danslapman")
      )
    ),
    licenses += ("WTFPL", url("http://www.wtfpl.net")),
    homepage := Some(url("https://github.com/danslapman/morphling"))
  )
}
