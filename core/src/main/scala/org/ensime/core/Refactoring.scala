// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.nio.charset.Charset

import org.ensime.api._
import org.ensime.util.FileUtils._
import org.ensime.util._
import org.ensime.util.file.File

import scala.concurrent.{ ExecutionContext, Future }
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring._
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.common.{ Change, CompilerAccess, RenameSourceFileChange }
import scala.tools.refactoring.implementations._

abstract class RefactoringEnvironment(file: String, start: Int, end: Int) {

  val refactoring: MultiStageRefactoring with CompilerAccess

  def performRefactoring(
    procId: Int,
    tpe: RefactorType,
    parameters: refactoring.RefactoringParameters
  )(implicit charset: Charset): Either[RefactorFailure, RefactorDiffEffect] = {

    def transformToDiff(modifications: List[Change]): Either[RefactorFailure, RefactorDiffEffect] = {
      val renameChange = modifications.collectFirst { case ch: RenameSourceFileChange => ch }
      val renameTarget = renameChange.map {
        ch =>
          val sourceFile = ch.sourceFile
          val fullNewName = sourceFile.path.replace(sourceFile.name, ch.to)
          (sourceFile.file, File(fullNewName))
      }
      val edits = modifications.flatMap(FileEditHelper.fromChange).sorted

      writeDiffChanges(edits, renameTarget) match {
        case Right(diff) => Right(RefactorDiffEffect(procId, tpe, diff))
        case Left(err) => Left(RefactorFailure(procId, err.toString))
      }
    }

    val af = AbstractFile.getFile(file)

    refactoring.compilationUnitOfFile(af) match {
      case Some(cu) =>
        val selection = new refactoring.FileSelection(af, cu.body, start, end)
        refactoring.prepare(selection) match {
          case Right(prepare) =>
            refactoring.perform(selection, prepare, parameters) match {
              case Right(modifications) => transformToDiff(modifications)
              case Left(error) => Left(RefactorFailure(procId, error.cause))
            }
          case Left(error) => Left(RefactorFailure(procId, error.cause))
        }
      case None =>
        Left(RefactorFailure(procId, "Compilation unit not found: " + af))
    }

  }
}

trait RefactoringHandler { self: Analyzer =>

  implicit def cs: Charset = charset

  def handleRefactorRequest(req: RefactorReq)(implicit ec: ExecutionContext): Future[RpcResponse] =
    scalaCompiler.askDoRefactor(req.procId, req.params).map {
      case Right(success) => success
      case Left(failure) => failure
    }
}

trait RefactoringControl { self: RichCompilerControl with RefactoringImpl =>

  def askDoRefactor(
    procId: Int,
    refactor: RefactorDesc
  )(
    implicit
    ec: ExecutionContext
  ): Future[Either[RefactorFailure, RefactorDiffEffect]] = doRefactor(procId, refactor)

}

trait RefactoringImpl {
  self: RichPresentationCompiler =>

  import org.ensime.util.FileUtils._

  protected def doRename(procId: Int, tpe: RefactorType, name: String, file: File, start: Int, end: Int, files: Set[String]) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new Rename with GlobalIndexes {
        val global = RefactoringImpl.this
        val invalidSet = toBeRemoved.synchronized { toBeRemoved.toSet }
        val cuIndexes: List[CompilationUnitIndex] = this.global.unitOfFile.collect {
          case (f, unit) if search.noReverseLookups || files.contains(f.file.getPath) =>
            println(s"status = ${unit.status}")
            CompilationUnitIndex(unit.body)
        }(collection.breakOut)
        val index = GlobalIndex(cuIndexes)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doExtractMethod(procId: Int, tpe: RefactorType,
    name: String, file: File, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractMethod with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doExtractLocal(procId: Int, tpe: RefactorType,
    name: String, file: File, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doInlineLocal(procId: Int, tpe: RefactorType, file: File,
    start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new InlineLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes)
      }
      val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters())
    }.result

  // we probably want to allow users to customise this
  private def organizeImportOptions(refactoring: OrganizeImports) = {
    import refactoring.oiWorker.participants._
    List(
      SortImports,
      SortImportSelectors,
      RemoveDuplicates
    )
  }

  protected def doOrganizeImports(procId: Int, tpe: RefactorType, file: File) =
    new RefactoringEnvironment(file.getPath, 0, 0) {
      val refactoring = new OrganizeImports {
        val global = RefactoringImpl.this
      }

      val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters(
        options = organizeImportOptions(refactoring),
        config = Some(OrganizeImports.OrganizeImportsConfig(
          importsStrategy = Some(OrganizeImports.ImportsStrategy.CollapseImports),
          groups = List("java", "scala")
        ))
      ))
    }.result

  protected def doAddImport(procId: Int, tpe: RefactorType, qualName: String, file: File) = {
    val refactoring = new AddImportStatement {
      val global = RefactoringImpl.this
    }

    val af = AbstractFile.getFile(file.getPath)
    val modifications = refactoring.addImport(af, qualName)
    val edits = modifications.flatMap(FileEditHelper.fromChange)

    writeDiffChanges(edits)(charset) match {
      case Right(diff) => Right(RefactorDiffEffect(procId, tpe, diff))
      case Left(err) => Left(RefactorFailure(procId, err.toString))
    }
  }

  protected def reloadAndType(f: File) = reloadAndTypeFiles(List(this.createSourceFile(f.getPath)))

  protected def doRefactor(
    procId: Int,
    refactor: RefactorDesc
  )(
    implicit
    ec: ExecutionContext
  ): Future[Either[RefactorFailure, RefactorDiffEffect]] = {

    def askRefactorResult(
      op: => Either[RefactorFailure, RefactorDiffEffect]
    ): Future[Either[RefactorFailure, RefactorDiffEffect]] =
      Future.successful(
        askOption {
          try { op } catch {
            case e: Throwable =>
              logger.error("Error during refactor request: " + refactor, e)
              Left(RefactorFailure(procId, e.toString))
          }
        }.getOrElse(Left(RefactorFailure(procId, "Refactor call failed.")))
      )

    val tpe = refactor.refactorType

    refactor match {
      case InlineLocalRefactorDesc(file, start, end) =>
        askRefactorResult {
          reloadAndType(file)
          doInlineLocal(procId, tpe, file, start, end)
        }
      case RenameRefactorDesc(newName, file, start, end) =>
        import scala.reflect.internal.util.{ RangePosition, OffsetPosition }
        val sourceFile = createSourceFile(file.getPath)
        askLoadedTyped(sourceFile)
        val pos = if (start == end) new OffsetPosition(sourceFile, start)
        else new RangePosition(sourceFile, start, start, end)
        val symbol = askSymbolAt(pos)
        symbol match {
          case None => Future.successful(Left(RefactorFailure(procId, "No symbol at given position.")))
          case Some(sym) =>
            usesOfSym(sym).map { uses =>
              val files = uses.map(rf => createSourceFile(rf)) - sourceFile
              files.foreach(askLoadedTyped)
              askOption(doRename(procId, tpe, newName, file, start, end, files.map(_.path) + sourceFile.path))
                .getOrElse(Left(RefactorFailure(procId, "Refactor call failed.")))
            }
        }
      case ExtractMethodRefactorDesc(methodName, file, start, end) =>
        askRefactorResult {
          reloadAndType(file)
          doExtractMethod(procId, tpe, methodName, file, start, end)
        }
      case ExtractLocalRefactorDesc(name, file, start, end) =>
        askRefactorResult {
          reloadAndType(file)
          doExtractLocal(procId, tpe, name, file, start, end)
        }
      case OrganiseImportsRefactorDesc(file) =>
        askRefactorResult {
          reloadAndType(file)
          doOrganizeImports(procId, tpe, file)
        }
      case AddImportRefactorDesc(qualifiedName, file) =>
        askRefactorResult {
          reloadAndType(file)
          doAddImport(procId, tpe, qualifiedName, file)
        }
    }
  }

}
