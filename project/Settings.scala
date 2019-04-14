import bintray.BintrayKeys._
import sbt._
import sbt.Keys._

object Settings {
  val common = Seq(
    organization := "danslapman",
    version := "1.0-beta4",
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.12.8"),
    scalacOptions ++= Seq(
      "-language:higherKinds",
      "-Ypartial-unification",
      "-Ywarn-unused:imports"
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
    licenses += ("WTFPL", url("http://www.wtfpl.net")),
    bintrayOrganization := Some("danslapman"),
    bintrayReleaseOnPublish in ThisBuild := false
  )
}
