import bintray.BintrayKeys._
import sbt._
import sbt.Keys._
import scalafix.sbt.ScalafixPlugin.autoImport._

object Settings {
  val common = Seq(
    organization := "danslapman",
    version := "1.0-beta13",
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.12.8"),
    scalacOptions ++= Seq(
      "-language:higherKinds,implicitConversions",
      "-Ypartial-unification",
      "-Ywarn-unused:imports"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.1"),
    addCompilerPlugin(scalafixSemanticdb),
    licenses += ("WTFPL", url("http://www.wtfpl.net")),
    bintrayOrganization := Some("danslapman"),
    bintrayReleaseOnPublish in ThisBuild := false
  )
}
