// Copyright 2016 Sam Halliday
// License: http://www.apache.org/licenses/LICENSE-2.0
import com.typesafe.sbt.SbtScalariform._
import java.util.concurrent.atomic.AtomicLong
import sbt.Keys._
import sbt._

import scala.util.Properties

/**
 * A bunch of sensible defaults that fommil typically uses.
 */
object Sensible {

  // used for unique gclog naming
  private val forkCount = new AtomicLong()

  lazy val settings = Seq(
    ivyLoggingLevel := UpdateLogging.Quiet,
    conflictManager := ConflictManager.strict,

    scalacOptions in Compile ++= Seq(
      "-encoding", "UTF-8",
      "-target:jvm-1.7",
      "-feature",
      "-deprecation",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-Xlint",
      "-Yinline-warnings",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      //"-Ywarn-numeric-widen", // noisy
      //"-Ywarn-value-discard", // will require a lot of work
      "-Xfuture"
    ) ++ {
        if (scalaVersion.value.startsWith("2.11")) Seq("-Ywarn-unused-import")
        else Nil
      } ++ {
        // fatal warnings can get in the way during the DEV cycle
        if (sys.env.contains("CI")) Seq("-Xfatal-warnings")
        else Nil
      },
    javacOptions in (Compile, compile) ++= Seq(
      "-Xlint:all", "-Werror",
      "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
    ),

    javaOptions := Seq(
      "-XX:MaxMetaspaceSize=256m",
      "-Xss2m", "-Xms512m", "-Xmx512m",
      "-Dfile.encoding=UTF8",
      "-XX:+UseG1GC", "-XX:+UseStringDeduplication"
    ),
    javaOptions in run ++= yourkitAgent, // interferes with sockets

    maxErrors := 1,
    fork := true,

    concurrentRestrictions in Global := {
      val limited = Properties.envOrElse("SBT_TASK_LIMIT", "4").toInt
      Seq(Tags.limitAll(limited))
    },

    dependencyOverrides ++= Set(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scalap" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-xml" % scalaModulesVersion,
      "org.scala-lang.modules" %% "scala-parser-combinators" % scalaModulesVersion,
      "org.scalamacros" %% "quasiquotes" % quasiquotesVersion,
      "org.scalatest" %% "scalatest" % scalatestVersion
    ) ++ logback ++ guava ++ shapeless(scalaVersion.value)
  ) ++ inConfig(Test)(testSettings) ++ scalariformSettings

  def testSettings = Seq(
    parallelExecution := true,

    // one JVM per test suite
    fork := true,
    testForkedParallel := true,
    testGrouping := {
      val opts = ForkOptions(
        bootJars = Nil,
        javaHome = javaHome.value,
        connectInput = connectInput.value,
        outputStrategy = outputStrategy.value,
        runJVMOptions = javaOptions.value,
        workingDirectory = Some(baseDirectory.value),
        envVars = envVars.value
      )
      definedTests.value.map { test =>
        Tests.Group(test.name, Seq(test), Tests.SubProcess(opts))
      }
    },

    javaOptions ++= {
      if (sys.env.get("GC_LOGGING").isEmpty) Nil
      else {
        val base = (baseDirectory in ThisBuild).value
        val config = configuration.value
        val n = name.value
        val count = forkCount.incrementAndGet() // subject to task evaluation
        val out = { base / s"gc-$config-$n.log" }.getCanonicalPath
        Seq(
          // https://github.com/fommil/lions-share
          s"-Xloggc:$out",
          "-XX:+PrintGCDetails",
          "-XX:+PrintGCDateStamps",
          "-XX:+PrintTenuringDistribution",
          "-XX:+PrintHeapAtGC"
        )
      }
    },

    testOptions ++= noColorIfEmacs,
    testFrameworks := Seq(TestFrameworks.ScalaTest, TestFrameworks.JUnit)
  )

  val scalaModulesVersion = "1.0.4"
  
  def akkaVersion: Def.Initialize[String] = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, minor)) if minor >= 11 => "2.4.12"
      case Some((2, minor)) => "2.3.16"
      case _ => ???
    }
  }

  val scalatestVersion = "3.0.0"
  val logbackVersion = "1.7.21"
  val quasiquotesVersion = "2.0.1"
  val guavaVersion = "19.0"

  val macroParadise = Seq(
    compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  )
  def shapeless(scalaVersion: String) = {
    if (scalaVersion.startsWith("2.10.")) macroParadise
    else Nil
  } :+ "com.chuusai" %% "shapeless" % "2.3.2"
  val logback = Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    "org.slf4j" % "slf4j-api" % logbackVersion,
    "org.slf4j" % "jul-to-slf4j" % logbackVersion,
    "org.slf4j" % "jcl-over-slf4j" % logbackVersion
  )
  val guava = Seq(
    "com.google.guava" % "guava" % guavaVersion,
    "com.google.code.findbugs" % "jsr305" % "3.0.1" % "provided"
  )

  def testLibs(config: String = "test") = Def.setting(Seq(
    // janino 3.0.6 is not compatible and causes http://www.slf4j.org/codes.html#replay
    "org.codehaus.janino" % "janino" % "2.7.8" % config,
    "org.scalatest" %% "scalatest" % scalatestVersion % config,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.3.0" % config,
    "org.scalacheck" %% "scalacheck" % "1.13.4" % config,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion.value % config,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion.value % config
  ) ++ logback.map(_ % config))

  // e.g. YOURKIT_AGENT=/opt/yourkit/bin/linux-x86-64/libyjpagent.so
  val yourkitAgent = Properties.envOrNone("YOURKIT_AGENT").map { name =>
    val agent = file(name)
    require(agent.exists(), s"Yourkit agent specified ($agent) does not exist")
    Seq(s"-agentpath:${agent.getCanonicalPath}=quiet")
  }.getOrElse(Nil)

  // WORKAROUND: https://github.com/scalatest/scalatest/issues/511
  def noColorIfEmacs =
    if (sys.env.get("INSIDE_EMACS").isDefined)
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oWF"))
    else
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oF"))

}
