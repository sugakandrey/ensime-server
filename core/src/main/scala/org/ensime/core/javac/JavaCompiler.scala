// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import java.io.{ File, FileInputStream, InputStream }
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

import scala.collection.JavaConverters._

import akka.actor.ActorRef
import akka.event.slf4j.SLF4JLogging
import com.sun.source.tree.Scope
import com.sun.source.util.{ JavacTask, TreePath }
import com.sun.tools.javac.util.Abort
import javax.lang.model.`type`.{ TypeMirror }
import javax.tools._
import org.ensime.api._
import org.ensime.core.DocSigPair
import org.ensime.indexer.SearchService
import org.ensime.util.ReportHandler
import org.ensime.util.file._
import org.ensime.vfs._

class JavaCompiler(
  val config: EnsimeConfig,
  val reportHandler: ReportHandler,
  val indexer: ActorRef,
  val search: SearchService,
  val vfs: EnsimeVFS
) extends JavaDocFinding
    with JavaCompletionsAtPoint
    with JavaTypeAtPoint
    with JavaSymbolAtPoint
    with JavaSourceFinding
    with Helpers
    with SLF4JLogging {

  private val listener = new JavaDiagnosticListener()
  private val silencer = new SilencedDiagnosticListener()
  private val cp = (config.allJars ++ config.targetClasspath).mkString(File.pathSeparator)
  private var workingSet = new ConcurrentHashMap[String, JavaFileObject]()

  // Cache the filemanager so we can re-use jars on the classpath. This has
  // *very* large performance implications: ensime-server/issues/1262
  //
  // Warning from docs: "An object of this interface is not required to
  //   support multi-threaded access, that is, be synchronized."
  var sharedFileManager: Option[JavaFileManager] = None

  def getTask(
    lint: String,
    listener: DiagnosticListener[JavaFileObject],
    files: java.lang.Iterable[JavaFileObject]
  ): JavacTask = {
    val compiler = ToolProvider.getSystemJavaCompiler()

    // Try to re-use file manager.
    // Note: we can't re-use the compiler, as that will
    // explode with 'Compilation in progress' on jdk6.
    // see comments on ensime-server/pull/1108
    val fileManager = sharedFileManager match {
      case Some(fm) => fm
      case _ =>
        val fm = compiler.getStandardFileManager(listener, null, DefaultCharset)
        sharedFileManager = Some(fm)
        fm
    }

    compiler.getTask(null, fileManager, listener, List(
      "-cp", cp, "-Xlint:" + lint, "-proc:none"
    ).asJava, null, files).asInstanceOf[JavacTask]
  }

  def internSource(sf: SourceFileInfo): JavaFileObject = {
    val jfo = getJavaFileObject(sf)
    workingSet.put(sf.file.getAbsolutePath, jfo)
    jfo
  }

  def askTypecheckFiles(files: List[SourceFileInfo]): Unit = {
    reportHandler.clearAllJavaNotes()
    for (sf <- files) {
      internSource(sf)
    }
    typecheckAll()
  }

  def askLinkPos(fqn: JavaFqn, file: SourceFileInfo): Option[SourcePosition] = {
    val infos = typecheckForUnits(List(file))
    infos.headOption.flatMap { c => findInCompiledUnit(c, fqn) }
  }

  def askDocSignatureAtPoint(file: SourceFileInfo, offset: Int): Option[DocSigPair] = {
    pathToPoint(file, offset) flatMap {
      case (c: Compilation, path: TreePath) =>
        docSignature(c, path)
    }
  }

  protected def pathToPoint(file: SourceFileInfo, offset: Int): Option[(Compilation, TreePath)] = {
    val infos = typecheckForUnits(List(file))
    infos.headOption.flatMap { c =>
      val path: Option[TreePath] = PathFor(c, offset)
      path.map { p => (c, p) }
    }
  }

  protected def scopeForPoint(file: SourceFileInfo, offset: Int): Option[(Compilation, Scope)] = {
    val infos = typecheckForUnits(List(file))
    infos.headOption.flatMap { c =>
      val path: Option[Scope] = ScopeFor(c, offset)
      path.map { p => (c, p) }
    }
  }

  private def getTypeMirror(c: Compilation, offset: Int): Option[TypeMirror] = {
    val path: Option[TreePath] = PathFor(c, offset)
    // Uncomment to debug the AST path.
    //for (p <- path) { for (t <- p) { System.err.println(t.toString()) } }
    path.flatMap { p => Option(c.trees.getTypeMirror(p)) }
  }

  private def typecheckAll(): Unit = {
    val task = getTask("all", listener, workingSet.values)
    val t = System.currentTimeMillis()
    try {
      task.parse()
      task.analyze()
      log.info("Parsed and analyzed: " + (System.currentTimeMillis() - t) + "ms")
    } catch {
      case e @ (_: Abort | _: ArrayIndexOutOfBoundsException | _: AssertionError) =>
        log.error("Javac error: " + e.getMessage())
    }
  }

  private def typecheckForUnits(inputs: List[SourceFileInfo]): Vector[Compilation] = {
    // We only want the compilation units for inputs, but we need to typecheck them w.r.t
    // the full working set.
    val inputJfos = inputs.map { sf => internSource(sf).toUri }.toSet
    val task = getTask("none", silencer, workingSet.values)
    val t = System.currentTimeMillis()
    try {
      val units = task.parse().asScala.filter { unit => inputJfos.contains(unit.getSourceFile.toUri) }
        .map(Compilation(task, _)).toVector
      task.analyze()
      log.info("Parsed and analyzed for trees: " + (System.currentTimeMillis() - t) + "ms")
      units
    } catch {
      case e @ (_: Abort | _: ArrayIndexOutOfBoundsException | _: AssertionError) =>
        log.error("Javac error: " + e.getMessage())
        Vector()
    }
  }

  private class JavaObjectWithContents(val f: File, val contents: String)
      extends SimpleJavaFileObject(f.toURI, JavaFileObject.Kind.SOURCE) {
    override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = contents
  }

  private class JavaObjectFromFile(val f: File)
      extends SimpleJavaFileObject(f.toURI, JavaFileObject.Kind.SOURCE) {
    override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = f.readString
    override def openInputStream(): InputStream = new FileInputStream(f)
  }

  private def getJavaFileObject(sf: SourceFileInfo): JavaFileObject = sf match {
    case SourceFileInfo(f, None, None) => new JavaObjectFromFile(f)
    case SourceFileInfo(f, Some(contents), None) => new JavaObjectWithContents(f, contents)
    case SourceFileInfo(f, None, Some(contentsIn)) => new JavaObjectWithContents(f, contentsIn.readString)
  }

  private class JavaDiagnosticListener extends DiagnosticListener[JavaFileObject] with ReportHandler {
    def report(diag: Diagnostic[_ <: JavaFileObject]): Unit = {
      reportHandler.reportJavaNotes(List(
        Note(
          diag.getSource().getName(),
          diag.getMessage(Locale.ENGLISH),
          diag.getKind() match {
            case Diagnostic.Kind.ERROR => NoteError
            case Diagnostic.Kind.WARNING => NoteWarn
            case Diagnostic.Kind.MANDATORY_WARNING => NoteWarn
            case _ => NoteInfo
          },
          diag.getStartPosition() match {
            case x if x > -1 => x.toInt
            case _ => diag.getPosition().toInt
          },
          diag.getEndPosition().toInt,
          diag.getLineNumber().toInt,
          diag.getColumnNumber().toInt
        )
      ))
    }
  }

  private class SilencedDiagnosticListener extends DiagnosticListener[JavaFileObject] with ReportHandler {
    def report(diag: Diagnostic[_ <: JavaFileObject]): Unit = {}
  }
}
