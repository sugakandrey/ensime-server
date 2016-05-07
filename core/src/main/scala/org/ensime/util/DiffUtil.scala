// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

object DiffUtil {

  def compareContents(original: Seq[String], revised: Seq[String], originalFile: File = new File("a"), revisedFile: File = new File("b")): String = {
    import collection.JavaConverters._
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    val originalInfo = originalFile.getAbsolutePath() + "\t" + fileModificationTimeOrEpoch(originalFile)
    val revisedInfo = revisedFile.getAbsolutePath() + "\t" + fileModificationTimeOrEpoch(revisedFile)
    if (diff.getDeltas.isEmpty) ""
    else difflib.DiffUtils.generateUnifiedDiff(originalInfo, revisedInfo, original.asJava, diff, 1).asScala.mkString("", "\n", "\n")
  }

  def fileModificationTimeOrEpoch(file: File): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss Z")
    if (file.exists)
      format.format(new Date(file.lastModified()))
    else {
      epoch
    }
  }

  private val epoch: String = "1970-01-01 12:00:00 +0000"

  def newFileDiff(text: Seq[String], file: File): String = {
    val diff = StringBuilder.newBuilder
    val originalInfo = s"${file.getAbsolutePath}\t$epoch"
    val revisedInfo = s"${file.getAbsolutePath}\t${fileModificationTimeOrEpoch(file)}"

    diff ++= s"--- $originalInfo\n"
    diff ++= s"+++ $revisedInfo\n"
    val additions = text.length
    diff ++= s"@@ -0,0 +1,$additions @@\n"
    diff ++= text.map(line => s"+$line").mkString("", "\n", "\n")
    diff.toString
  }

  def deleteFileDiff(text: Seq[String], file: File): String = {
    val diff = StringBuilder.newBuilder
    val originalInfo = s"${file.getAbsolutePath}\t${fileModificationTimeOrEpoch(file)}"
    val revisedInfo = s"${file.getAbsolutePath}\t$epoch"

    diff ++= s"--- $originalInfo\n"
    diff ++= s"+++ $revisedInfo\n"
    val removals = text.length
    diff ++= s"@@ -1,$removals +0,0 @@\n"
    diff ++= text.map(line => s"-$line").mkString("", "\n", "\n")
    diff.toString
  }
}
