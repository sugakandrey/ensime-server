// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import scala.collection.JavaConverters._
import java.io.File
import java.util.concurrent.ConcurrentHashMap

import org.ensime.api._
import org.ensime.config._
import org.ensime.util.ensimefile._
import org.ensime.util.file._
import org.scaladebugger.api.profiles.traits.info.LocationInfo

import scala.collection.mutable

/**
 * Represents a utility to map local source files provided by Ensime to
 * JDI locations.
 *
 * @param config The Ensime configuration used to load source files
 * @param pathMap Represents a cache of files indexed by short file paths such
 *                as file.scala rather than org/ensime/file.scala
 */
@deprecating("this is duplicating the functionality of the indexer and source resolver")
class SourceMap(
    private val config: EnsimeConfig,
    private val pathMap: mutable.Map[String, File] = new ConcurrentHashMap[String, File]().asScala
) {
  /** Contains a collection of root paths where source files are located */
  private lazy val roots: Seq[String] = retrieveRoots

  /** Contains a set of local Scala source files */
  private lazy val sources: Set[File] = retrieveSources

  /** Contains a mapping of filename (file.scala) to local file */
  private lazy val sourceMap: Map[String, Set[File]] = sources.groupBy(_.getName)

  /**
   * Creates a new LineSourcePosition instance from the given location.
   *
   * @param location The location to use when constructing the new
   *                 LineSourcePosition
   * @return Some LineSourcePosition if matching source file is found,
   *         otherwise None
   */
  def newLineSourcePosition(
    location: LocationInfo
  ): Option[LineSourcePosition] = {
    findFileByLocation(location).map(f =>
      LineSourcePosition(f, location.lineNumber))
  }

  /**
   * Finds the local source file mapping to the given location.
   *
   * @param location The location whose source file to find
   * @return Some file representing the local source, otherwise None
   */
  def findFileByLocation(location: LocationInfo): Option[EnsimeFile] =
    location.trySourcePath.toOption.flatMap(sourceForFilePath).map(EnsimeFile)

  /**
   * Retrieves all current Scala sources available through Ensime with the
   * given file name.
   *
   * @param fileName The name of the file whose matches to retrieve
   * @return The set of sources whose file name match the given name
   */
  def sourcesForFileName(fileName: String): Set[File] =
    sourceMap.getOrElse(fileName, Set())

  /**
   * Retrieves the current Scala source available through Ensime with the
   * given file path.
   *
   * @param filePath The path of the file whose match to retrieve
   * @return Some source whose file path matches the given path, otherwise None
   */
  def sourceForFilePath(filePath: String): Option[File] = {
    // Check if we have a match in the cached path map first
    val cachedResult = pathMap.get(filePath)

    // If no cached result, search through all of sources to find a match
    val result = sources.find(_.getAbsolutePath.endsWith(filePath))

    // Store the found result as our new cached result
    if (cachedResult.isEmpty && result.nonEmpty)
      pathMap.put(filePath, result.get)

    cachedResult.orElse(result)
  }

  /**
   * Retrieves current Scala sources available through Ensime.
   *
   * @return The set of Scala source files
   */
  def canonicalSources: Set[File] = sources

  /**
   * Parses the canonical path of the provided file, removing the root path
   * and leaving the relative source path for use by breakpoints.
   *
   * @param file The file whose path to parse
   * @return The relative source path
   */
  def parsePath(file: File): String = {
    parsePath(file.getCanonicalPath)
  }

  /**
   * Parses the file path, removing the root path and leaving the relative
   * source path for use by breakpoints.
   *
   * @param filePath The absolute file path
   * @return The relative source path
   */
  def parsePath(filePath: String): String = {
    parsePath(roots, filePath)
  }

  /**
   * Parses a source path, removing the matching root path from the source path.
   *
   * @param rootPaths The root paths to remove from the source path
   * @param sourcePath The source path to strip of the root path
   * @return The stripped source path
   */
  private def parsePath(rootPaths: Seq[String], sourcePath: String): String = {
    rootPaths.find(sourcePath.startsWith).map(p => sourcePath.replace(p, ""))
      .getOrElse(sourcePath).stripPrefix(java.io.File.separator)
  }

  /**
   * Retrieves a collection of file paths representing the root locations of
   * source files managed by Ensime.
   *
   * @return The distinct root paths as strings
   */
  protected def retrieveRoots: Seq[String] = (
    config.compileClasspath.map(_.getCanonicalPath).toSeq ++
    config.javaSources.map(_.getCanonicalPath) ++
    config.subprojects.flatMap(_.sourceRoots).map(_.getCanonicalPath)
  ).distinct

  /**
   * Retrieves a set of local files representing available Scala source files
   * managed by Ensime.
   *
   * @return The set of local files
   */
  protected def retrieveSources: Set[File] = config.scalaSourceFiles.map(_.canon)
}
