import java.io._
import scala.util.{ Properties, Try }

import com.typesafe.sbt.SbtScalariform.autoImport._
import com.typesafe.sbt.SbtScalariform
import scalariform.formatter.preferences._

import de.heikoseeberger.sbtheader.{ HeaderKey, HeaderPlugin }
import sbt.Keys._
import sbt.{ IntegrationTest => It, _ }
import sbtassembly.AssemblyKeys._
import sbtassembly.{ AssemblyKeys, MergeStrategy, PathList }
import sbtbuildinfo.BuildInfoPlugin, BuildInfoPlugin.autoImport._

import org.ensime.EnsimePlugin.JdkDir
import org.ensime.EnsimeKeys._

import fommil.SensibleSettings._
import fommil.SonatypeKeys._
 
object ProjectPlugin extends AutoPlugin {
  override def requires = fommil.SensiblePlugin
  override def trigger = allRequirements

  override def buildSettings = Seq(
    scalaVersion := "2.11.8",
    organization := "org.ensime",
    version := "2.0.0-SNAPSHOT",

    ensimeIgnoreMissingDirectories := true,

    sonatypeGithub := ("ensime", "ensime-server"),
    licenses := Seq(GPL3),
    startYear := Some(2010)
  )

  override def projectSettings = Seq(
    scalariformPreferences := SbtScalariform.defaultPreferences
  )
}

object EnsimeBuild {
  // common to the ensimeBuild, but not the testing projects
  lazy val commonSettings = Seq(
    dependencyOverrides ++= Set(
       "com.typesafe.akka" %% "akka-actor" % akkaVersion.value,
       "com.typesafe.akka" %% "akka-testkit" % akkaVersion.value,
       "io.spray" %% "spray-json" % "1.3.2"
    ),

    // disabling shared memory gives a small performance boost to tests
    javaOptions ++= Seq(
      "-XX:+PerfDisableSharedMem",
      "-Xms1g",
      "-Xmx1g",
      "-XX:MaxDirectMemorySize=4g"
    ),


    dependencyOverrides ++= Set(
      "org.apache.lucene" % "lucene-core" % luceneVersion
    ),

    updateOptions := updateOptions.value.withCachedResolution(true)
  )

  lazy val commonItSettings = inConfig(It)(
    Defaults.testSettings ++ sensibleTestSettings
  ) ++ SbtScalariform.scalariformSettingsWithIt ++ HeaderPlugin.settingsFor(It)

  lazy val JavaTools: File = JdkDir / "lib/tools.jar"

  ////////////////////////////////////////////////
  // modules
  lazy val monkeys = Project("monkeys", file("monkeys")) settings (commonSettings) settings (
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.apache.commons" % "commons-vfs2" % "2.1" exclude ("commons-logging", "commons-logging")
    )
  )

  lazy val api = Project("api", file("api")) settings (commonSettings) settings (
    licenses := Seq(Apache2)
  )

  lazy val util = Project("util", file("util")) settings (commonSettings) dependsOn (
    api
  ) settings (
    libraryDependencies ++= List(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.apache.commons" % "commons-vfs2" % "2.1" exclude ("commons-logging", "commons-logging"),
      "com.google.guava" % "guava" % "20.0",
      "com.google.code.findbugs" % "jsr305" % "3.0.1" % "provided"
    ) ++ logback
  )

  lazy val testutil = Project("testutil", file("testutil")) settings (commonSettings) dependsOn (
    util, api
  ) settings (
      libraryDependencies ++= Seq(
        "commons-io" % "commons-io" % "2.5",
        "com.typesafe.akka" %% "akka-testkit" % akkaVersion.value,
        "com.typesafe.akka" %% "akka-slf4j" % akkaVersion.value
      ) ++sensibleTestLibs(Compile)
    )

  lazy val s_express = Project("s-express", file("s-express")) settings (commonSettings) settings (
      licenses := Seq(LGPL3),
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "fastparse" % "0.4.2",
        "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
      ) ++ shapeless.value ++ logback
    )

  // the JSON protocol
  lazy val jerky = Project("jerky", file("protocol-jerky")) settings (commonSettings) dependsOn (
    util,
    api,
    testutil % Test,
    api % "test->test" // for the test data
  ) settings (
      libraryDependencies ++= Seq(
        "com.github.fommil" %% "spray-json-shapeless" % "1.3.0",
        "com.typesafe.akka" %% "akka-slf4j" % akkaVersion.value
      ) ++ shapeless.value
    )

  // the S-Exp protocol
  lazy val swanky = Project("swanky", file("protocol-swanky")) settings (commonSettings) dependsOn (
    api, s_express, util,
    testutil % Test,
    api % "test->test" // for the test data
  ) settings (
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-slf4j" % akkaVersion.value
      ) ++ shapeless.value
    )

  import EnsimeTestingBuild._
  lazy val core = Project("core", file("core")).dependsOn(
    api, s_express, monkeys, util,
    api % "test->test", // for the interpolator
    testutil % "test,it",
    // depend on "it" dependencies in Test or sbt adds them to the release deps!
    // https://github.com/sbt/sbt/issues/1888
    testingEmpty % "test,it",
    testingSimple % "test,it",
    // test config needed to get the test jar
    testingSimpleJar % "test,it->test",
    testingTiming % "test,it",
    testingMacros % "test, it",
    testingShapeless % "test,it",
    testingDebug % "test,it",
    testingJava % "test,it"
  ).configs(It).settings(
      commonSettings, commonItSettings
    ).settings(
      unmanagedJars in Compile += JavaTools,
      ensimeUnmanagedSourceArchives += (baseDirectory in ThisBuild).value / "openjdk-langtools/openjdk8-langtools-src.zip",
      libraryDependencies ++= Seq(
        "com.orientechnologies" % "orientdb-graphdb" % orientVersion
          exclude ("commons-collections", "commons-collections")
          exclude ("commons-beanutils", "commons-beanutils")
          exclude ("commons-logging", "commons-logging"),
        "org.apache.lucene" % "lucene-core" % luceneVersion,
        "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
        "org.ow2.asm" % "asm-commons" % "5.1",
        "org.ow2.asm" % "asm-util" % "5.1",
        "org.scala-lang" % "scalap" % scalaVersion.value,
        "com.typesafe.akka" %% "akka-actor" % akkaVersion.value,
        "com.typesafe.akka" %% "akka-slf4j" % akkaVersion.value,
        scalaBinaryVersion.value match {
          // see notes in https://github.com/ensime/ensime-server/pull/1446
          case "2.10" => "org.scala-refactoring" % "org.scala-refactoring.library_2.10.6" % "0.11.0"
          case "2.11" => "org.scala-refactoring" % "org.scala-refactoring.library_2.11.8" % "0.11.0"
        },
        "commons-lang" % "commons-lang" % "2.6",
        "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
        "org.scala-debugger" %% "scala-debugger-api" % "1.1.0-M2",
        "org.scalamock" %% "scalamock-scalatest-support" % "3.4.2" % Test
      ) ++ shapeless.value
    ) enablePlugins BuildInfoPlugin settings (
        buildInfoPackage := organization.value,
        buildInfoKeys += BuildInfoKey.action("gitSha")(Try("git rev-parse --verify HEAD".!! dropRight 1) getOrElse "n/a"),
        buildInfoKeys += BuildInfoKey.action("builtAtString")(currentDateString())
      )

  private def currentDateString() = {
    val dtf = new java.text.SimpleDateFormat("yyyy-MM-dd")
    dtf.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    dtf.format(new java.util.Date())
  }

  val luceneVersion = "6.3.0"
  val nettyVersion = "4.1.6.Final"
  lazy val server = Project("server", file("server")).dependsOn(
    core, swanky, jerky,
    s_express % "test->test",
    swanky % "test->test",
    // depend on "it" dependencies in Test or sbt adds them to the release deps!
    // https://github.com/sbt/sbt/issues/1888
    core % "test->test",
    core % "it->it",
    testingDocs % "test,it"
  ).configs(It).settings(
      commonSettings ++ commonItSettings
    ).settings(
        libraryDependencies ++= Seq(
          "io.netty"    %  "netty-transport"  % nettyVersion,
          "io.netty"    %  "netty-handler"    % nettyVersion,
          "io.netty"    %  "netty-codec-http" % nettyVersion
        ) ++ shapeless.value
      )

  // manual root project so we can exclude the testing projects from publication
  lazy val root = Project(id = "ensime", base = file(".")) settings (commonSettings) aggregate (
    api, monkeys, util, s_express, jerky, swanky, core, server
  ) dependsOn (server) settings (
      // e.g. `sbt ++2.11.8 ensime/assembly`
      test in assembly := {},
      sourceDirectories in Compile := Nil,
      resources in Compile := Nil,
      aggregate in assembly := false,
      assemblyMergeStrategy in assembly := {
        case PathList("org", "apache", "commons", "vfs2", xs @ _*) => MergeStrategy.first // assumes our classpath is setup correctly
        case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.concat // assumes our classpath is setup correctly
        case PathList("LICENSE") => MergeStrategy.concat // WORKAROUND https://github.com/sbt/sbt-assembly/issues/224
        case other => MergeStrategy.defaultMergeStrategy(other)
      },
      assemblyExcludedJars in assembly := {
        val everything = (fullClasspath in assembly).value
        everything.filter { attr =>
          val n = attr.data.getName
          n.startsWith("scala-library") | n.startsWith("scala-compiler") |
            n.startsWith("scala-reflect") | n.startsWith("scalap")
        } :+ Attributed.blank(JavaTools)
      },
      assemblyJarName in assembly := s"ensime_${scalaBinaryVersion.value}-${version.value}-assembly.jar"
    )

  private def akkaVersion: Def.Initialize[String] = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, minor)) if minor >= 11 => "2.4.16"
      case _ => "2.3.16"
    }
  }
  
  private val orientVersion = "2.2.13"
}

// projects used in the integration tests, not published
object EnsimeTestingBuild {
  private def testingProject(dir: String) = Project(dir.replace("/", "_"), file(dir)).settings(
    scalacOptions := Nil,
    libraryDependencies := Seq("org.scala-lang" % "scala-library" % scalaVersion.value)
  )

  lazy val testingEmpty = testingProject("testing/empty")

  lazy val testingSimple = testingProject("testing/simple") settings (
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test intransitive ()
  )

  lazy val testingSimpleJar = testingProject("testing/simpleJar").settings(
    exportJars := true,
    ensimeUseTarget in Compile := Some((artifactPath in (Compile, packageBin)).value),
    ensimeUseTarget in Test := Some((artifactPath in (Test, packageBin)).value)
  )

  lazy val testingImplicits = testingProject("testing/implicits")

  lazy val testingTiming = testingProject("testing/timing")

  lazy val testingMacros = testingProject("testing/macros") settings (
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

  // just to have access to shapeless
  lazy val testingShapeless = testingProject("testing/shapeless").settings (
    libraryDependencies ++= shapeless.value
  )

  lazy val testingFqns = testingProject("testing/fqns").settings (
    libraryDependencies ++= shapeless.value ++ Seq(
      "org.typelevel" %% "cats" % "0.6.0" % Test intransitive(),
      "org.spire-math" %% "spire" % "0.11.0" % Test intransitive()
    )
  )

  lazy val testingDebug = testingProject("testing/debug")

  lazy val testingDocs = testingProject("testing/docs").settings(
    dependencyOverrides ++= Set("com.google.guava" % "guava" % "18.0"),
    libraryDependencies ++= Seq(
      "com.github.dvdme" % "ForecastIOLib" % "1.5.1" intransitive (),
      "commons-io" % "commons-io" % "2.5" intransitive (),
      "com.google.guava" % "guava" % "18.0"
    )
  )

  // java project with no scala-library
  lazy val testingJava = testingProject("testing/java").settings(
    crossPaths := false,
    autoScalaLibrary := false,
    libraryDependencies := Nil
  )
}
