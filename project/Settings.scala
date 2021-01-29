import bintray.BintrayKeys._
import sbt._
import sbt.Keys._
import scalafix.sbt.ScalafixPlugin.autoImport._

object Settings {
  val common = Seq(
    organization := "danslapman",
    version := "2.5.2",
    scalaVersion := "2.13.4",
    crossScalaVersions := Seq("2.12.12", "2.13.4"),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) => Seq(
          "-language:higherKinds,implicitConversions",
          "-Ywarn-unused:imports",
          "-deprecation",
          "-Ypartial-unification"
        )
        case Some((2, 13)) => Seq(
          "-language:higherKinds,implicitConversions",
          "-Ywarn-unused:imports",
          "-deprecation",
          "-Ymacro-annotations"
        )
        case _ => Seq()
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(
          compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full),
          compilerPlugin(scalafixSemanticdb)
        )
        case _ =>
          Seq.empty[ModuleID]
      }
    },
    scalafixDependencies in ThisBuild ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(
          "com.github.liancheng" %% "organize-imports" % "0.4.4"
        )
        case _ =>
          Seq.empty[ModuleID]
      }
    },
    licenses += ("WTFPL", url("http://www.wtfpl.net")),
    bintrayOrganization := Some("danslapman"),
    bintrayReleaseOnPublish in ThisBuild := false
  )
}
