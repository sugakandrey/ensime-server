// sbt-ensime is needed for the integration tests
addSbtPlugin("org.ensime" % "sbt-ensime" % "1.10.0")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "1.5.1")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")

scalacOptions in Compile ++= Seq("-feature", "-deprecation")

ivyLoggingLevel := UpdateLogging.Quiet

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M13")
