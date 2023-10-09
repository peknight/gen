ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / organization := "com.peknight"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-Xfatal-warnings",
    "-language:strictEquality",
    "-Xmax-inlines:64"
  ),
)

lazy val gen = (project in file("."))
  .aggregate(
    genCore.jvm,
    genCore.js,
    genCharsets.jvm,
    genCharsets.js,
  )
  .settings(commonSettings)
  .settings(
    name := "gen",
  )

lazy val genCore = (crossProject(JSPlatform, JVMPlatform) in file("gen-core"))
  .settings(commonSettings)
  .settings(
    name := "gen-core",
    libraryDependencies ++= Seq(
      "com.peknight" %%% "random-core" % pekRandomVersion,
    ),
  )

lazy val genCharsets = (crossProject(JSPlatform, JVMPlatform) in file("gen-charsets"))
  .dependsOn(genCore % Test)
  .settings(commonSettings)
  .settings(
    name := "gen-charsets",
    libraryDependencies ++= Seq(
      "com.peknight" %%% "random-core" % pekRandomVersion,
      "com.peknight" %%% "validation-spire" % pekValidationVersion,
      "com.peknight" %%% "spire-ext" % pekSpireExtVersion,
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test,
      "com.peknight" %%% "cats-instances-scalacheck" % pekInstancesVersion % Test,
    ),
  )

val pekVersion = "0.1.0-SNAPSHOT"
val pekRandomVersion = pekVersion
val pekValidationVersion = pekVersion
val pekSpireExtVersion = pekVersion
val pekInstancesVersion = pekVersion

val scalaCheckVersion = "1.17.0"
