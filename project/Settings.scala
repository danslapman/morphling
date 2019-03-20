import bintray.BintrayKeys._
import sbt._
import sbt.Keys._

object Settings {
  val common = Seq(
    organization := "danslapman",
    version := "0.1.0",
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8"),
    scalacOptions += "-Ypartial-unification",
    scalacOptions += "-language:higherKinds",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
    licenses += ("WTFPL", url("http://www.wtfpl.net")),
    bintrayOrganization := Some("danslapman"),
    bintrayReleaseOnPublish in ThisBuild := false
  )
}
