import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val gen = (project in file("."))
  .settings(name := "gen")
  .aggregate(
    genCore.jvm,
    genCore.js,
    genCharsets.jvm,
    genCharsets.js,
  )

lazy val genCore = (crossProject(JVMPlatform, JSPlatform) in file("gen-core"))
  .settings(name := "gen-core")
  .settings(crossDependencies(peknight.random))

lazy val genCharsets = (crossProject(JVMPlatform, JSPlatform) in file("gen-charsets"))
  .dependsOn(genCore % Test)
  .settings(name := "gen-charsets")
  .settings(crossDependencies(
    peknight.random,
    peknight.validation.spire,
    peknight.spire,
  ))
  .settings(crossTestDependencies(
    peknight.cats.scalaCheck,
    scalaTest.flatSpec,
    typelevel.catsEffect.testingScalaTest,
  ))
