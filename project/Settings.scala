import bintray.BintrayKeys._
import sbt._
import sbt.Keys._
import scalafix.sbt.ScalafixPlugin.autoImport._

object Settings {
  val common = Seq(
    organization := "danslapman",
    version := "2.0",
    scalaVersion := "2.13.1",
    crossScalaVersions := Seq("2.12.11", "2.13.1"),
    scalacOptions ++= Seq(
      "-language:higherKinds,implicitConversions",
      "-Ywarn-unused:imports",
      "-deprecation"
    ),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, y)) if y == 13 => Seq("-Ymacro-annotations")
        case _ => Seq("-Ypartial-unification")
      }
    },
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
    addCompilerPlugin(scalafixSemanticdb),
    //scalafixDependencies += "com.nequissimus" %% "sort-imports" % "0.3.2",
    licenses += ("WTFPL", url("http://www.wtfpl.net")),
    bintrayOrganization := Some("danslapman"),
    bintrayReleaseOnPublish in ThisBuild := false
  )
}
