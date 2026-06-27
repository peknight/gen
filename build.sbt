import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

ThisBuild / scalacOptions --= Seq("-Werror", "-Xfatal-warnings")

lazy val gen = (project in file("."))
  .settings(name := "gen")
  .aggregate(genCore.projectRefs *)
  .aggregate(genCharsets.projectRefs *)

lazy val genCore = (projectMatrix in file("gen-core"))
  .settings(name := "gen-core")
  .settings(libraryDependencies ++= dependencies(peknight.random))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val genCharsets = (projectMatrix in file("gen-charsets"))
  .dependsOn(genCore % Test)
  .settings(name := "gen-charsets")
  .settings(libraryDependencies ++= dependencies(
    peknight.random,
    peknight.validation.spire,
    peknight.spire,
  ))
  .settings(libraryDependencies ++= testDependencies(
    peknight.cats.scalaCheck,
    scalaTest.flatSpec,
    typelevel.catsEffect.testingScalaTest,
  ))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))
