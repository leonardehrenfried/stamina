import sbt._
import sbt.Keys._

object Build extends Build {
  import Dependencies._
  import Formatting._
  import Publishing._

  lazy val basicSettings = Seq(
    organization := "com.scalapenos",
    version := "0.1.3-SNAPSHOT",
    licenses := Seq("The MIT License (MIT)" -> url("http://opensource.org/licenses/MIT")),
    scalaVersion := "2.11.8",
    incOptions := incOptions.value.withNameHashing(true),
    scalacOptions := Seq(
      "-encoding", "utf8",
      "-target:jvm-1.8",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-language:_",
      "-Xlint",
      "-Xlog-reflective-calls",
      "-Ywarn-unused",
      "-Ywarn-unused-import"
    )
  )

  lazy val libSettings = basicSettings ++ formattingSettings ++ publishingSettings

  lazy val root = Project("stamina", file("."))
    .settings(basicSettings: _*)
    .settings(publishingSettings: _*)
    .aggregate(
      core,
      json,
      `play-json`,
      testkit
    )

  lazy val core = Project("stamina-core", file("stamina-core"))
    .settings(libSettings: _*)
    .settings(libraryDependencies ++=
      compile(
        akkaActor
      ) ++
      test(
        scalatest
      )
    )

  lazy val json = Project("stamina-json", file("stamina-json"))
    .dependsOn(core)
    .settings(libSettings: _*)
    .settings(libraryDependencies ++=
      compile(
        sprayJson,
        jsonLenses
      ) ++
      test(
        scalatest,
        sprayJsonShapeless
      )
    )

  lazy val `play-json` = Project("stamina-play-json", file("stamina-play-json"))
    .dependsOn(core)
    .settings(libSettings: _*)
    .settings(libraryDependencies ++=
      compile(
        playJson
      ) ++
      test(
        scalatest
      )
    )


  lazy val testkit = Project("stamina-testkit", file("stamina-testkit"))
    .dependsOn(core)
    .settings(libSettings: _*)
    .settings(libraryDependencies ++=
      compile(
        scalatest,
        base64
      )
    )
}
