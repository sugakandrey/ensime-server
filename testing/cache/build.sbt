import org.ensime.EnsimeKeys._
import org.ensime.EnsimeCoursierKeys._

/*
 This project exists as a standalone that can be used to generate an
 ensime cache of the java and scala stdlibs. With the bonus of
 integration testing the ensimeServerIndex feature.
 */

sonatypeGithub := ("ensime", "ensime-server-testing-cache")
licenses := Seq(Apache2)

// override sbt-sensible
libraryDependencies := Seq("org.scala-lang" % "scala-library" % scalaVersion.value)
libraryDependencies in Test := Nil

ensimeServerJars in ThisBuild := {
  val Some((major, minor)) = CrossVersion.partialVersion(scalaVersion.value)
  val ev = ensimeServerVersion.value
  val base = sys.env.get("SBT_VOLATILE_TARGET").map(_ + sys.props("user.dir")).getOrElse("./")
  val target = file(s"$base/../../target/scala-$major.$minor")
  val search = target ** s"ensime_$major.$minor-*"
  List(search.get.head.getCanonicalFile)
}
ensimeServerProjectJars := Nil
ensimeIgnoreMissingDirectories in ThisBuild := true
ensimeCachePrefix in ThisBuild := sys.env.get("SBT_VOLATILE_TARGET").map(ramdisk => file(ramdisk + "/ensime-cache"))
