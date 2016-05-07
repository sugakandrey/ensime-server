// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import org.ensime.api._
import org.ensime.util.file.File

import scala.tools.refactoring.common.{ NewFileChange, RenameSourceFileChange }

object FileEditHelper {

  import scala.tools.refactoring.common.{ Change, TextChange }

  def fromChange(ch: Change): Seq[FileEdit] = {
    ch match {
      case ch: TextChange => Seq(TextEdit(ch.file.file, ch.from, ch.to, ch.text))
      case nf: NewFileChange => Seq(NewFile(File(nf.fullName), nf.text))
      case rf: RenameSourceFileChange =>
        val sourceFile = rf.sourceFile
        val newFile = sourceFile.path.replace(sourceFile.name, rf.to)
        Seq(
          NewFile(File(newFile), ""),
          DeleteFile(sourceFile.file, "")
        )
      case _ => throw new UnsupportedOperationException(ch.toString)
    }
  }

  def applyEdits(ch: List[TextEdit], source: String): String = {
    (source /: ch.sortBy(-_.to)) { (src, change) =>
      src.substring(0, change.from) + change.text + src.substring(change.to)
    }
  }

  def diffFromTextEdits(ch: List[TextEdit], source: String, originalFile: File, revisedFile: File): String = {
    val newContents = applyEdits(ch, source)
    DiffUtil.compareContents(source.lines.toSeq, newContents.lines.toSeq, originalFile, revisedFile)
  }

  def diffFromNewFile(newFile: NewFile, sourceChanges: List[TextEdit] = Nil, source: String = ""): String = {
    val newContents = applyEdits(sourceChanges, source)
    DiffUtil.newFileDiff(newContents.lines.toSeq, newFile.file)
  }

  def diffFromDeleteFile(deletedFile: DeleteFile, source: String): String =
    DiffUtil.deleteFileDiff(source.lines.toSeq, deletedFile.file)

}
