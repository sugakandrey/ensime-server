// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import java.io.File
import java.io.File.{ separator => sep }

import scala.collection.mutable
import scala.util.Success

import org.ensime.api._
import org.ensime.util.EnsimeSpec
import org.ensime.util.ensimefile._
import org.scaladebugger.api.profiles.traits.info.LocationInfo
import org.scalamock.scalatest.MockFactory
import org.scalatest.OneInstancePerTest

class SourceMapSpec extends EnsimeSpec with OneInstancePerTest with MockFactory {
  private val testRoots = Seq(
    s"${sep}some${sep}file${sep}path",
    "other",
    "path"
  ).map(new File(_)).map(_.getCanonicalPath)
  private val testFileName = "file.scala"
  private val testSources = testRoots.map(_ + sep + testFileName).map(new File(_)).toSet
  private val testPathMap = new mutable.HashMap[String, File]()

  // NOTE: Unable to mock or extend EnsimeConfig (final) and too complicated to
  //       realistically provide default values to use in source map tests; so,
  //       overriding protected methods used to retrieve information from config
  private val sourceMap = new SourceMap(
    config = null,
    pathMap = testPathMap
  ) {
    override protected def retrieveRoots: Seq[String] = testRoots
    override protected def retrieveSources: Set[File] = testSources
  }

  "A SourceMap" should "be able to construct a LineSourcePosition from a location" in {
    val mockLocationInfo = mock[LocationInfo]
    val lineNumber = 999

    (mockLocationInfo.trySourcePath _).expects()
      .returning(Success(testSources.head.getPath)).once()

    (mockLocationInfo.lineNumber _).expects().returning(lineNumber).once()

    sourceMap.newLineSourcePosition(mockLocationInfo) should
      contain(LineSourcePosition(EnsimeFile(testSources.head), lineNumber))
  }

  it should "use the source path of a location to find the associated local file" in {
    val mockLocationInfo = mock[LocationInfo]

    (mockLocationInfo.trySourcePath _).expects()
      .returning(Success(testSources.head.getPath)).once()

    sourceMap.findFileByLocation(mockLocationInfo) should contain(EnsimeFile(testSources.head))
  }

  it should "be able to return all matching sources whose file name match the provided" in {
    sourceMap.sourcesForFileName(testFileName) should be(testSources)
  }

  it should "check requests for a source against the path map first" in {
    val expected = new File("anotherfile.scala")
    val filePath = "something"
    testPathMap.put(filePath, expected)

    sourceMap.sourceForFilePath(filePath) should contain(expected)
  }

  it should "look for files whose absolute paths end with the given file path" in {
    sourceMap.sourceForFilePath(testFileName) should contain(testSources.head)
  }

  it should "cache retrieved files given a shorter path in the path map" in {
    val result = sourceMap.sourceForFilePath(testFileName).get

    testPathMap should contain key (testFileName)
    testPathMap should contain value (result)
  }

  it should "be able to return a set of files representing scala sources" in {
    sourceMap.canonicalSources should be(testSources)
  }

  it should "be able to strip common roots from files" in {
    val matching = new File(testRoots.head + sep + testFileName)
    sourceMap.parsePath(matching) should be(testFileName)

    val nonMatching = new File("something" + sep + testFileName)
    sourceMap.parsePath(nonMatching) should
      be(nonMatching.getCanonicalPath.stripPrefix(java.io.File.separator))
  }

  it should "be able to strip common roots from file paths" in {
    val matching = testRoots.head + sep + testFileName
    sourceMap.parsePath(matching) should be(testFileName)

    val nonMatching = "something" + sep + testFileName
    sourceMap.parsePath(nonMatching) should be(nonMatching)
  }
}
