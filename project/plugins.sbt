// sbt-ensime is needed for the integration tests
addSbtPlugin("org.ensime" % "sbt-ensime" % "0.5.0")

// BUG https://github.com/sbt/sbt-header/issues/31
//addSbtPlugin("de.heikoseeberger" % "sbt-header" % "1.5.0")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.5.1")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.1")

scalacOptions in Compile ++= Seq("-feature", "-deprecation")

ivyLoggingLevel := UpdateLogging.Quiet

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.0")

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M12-1")
