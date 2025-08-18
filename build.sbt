import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val gen = (project in file("."))
  .aggregate(
    genCore.jvm,
    genCore.js,
    genCharsets.jvm,
    genCharsets.js,
  )
  .settings(
    name := "gen",
  )

lazy val genCore = (crossProject(JVMPlatform, JSPlatform) in file("gen-core"))
  .settings(crossDependencies(peknight.random))
  .settings(
    name := "gen-core",
  )

lazy val genCharsets = (crossProject(JVMPlatform, JSPlatform) in file("gen-charsets"))
  .dependsOn(genCore % Test)
  .settings(crossDependencies(
    peknight.random,
    peknight.validation.spire,
    peknight.ext.spire,
  ))
  .settings(crossTestDependencies(
    scalaCheck,
    peknight.instances.cats.scalaCheck,
    scalaTest.flatSpec,
    typelevel.catsEffect.testingScalaTest,
  ))
  .settings(
    name := "gen-charsets",
  )
