// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import java.nio.charset.Charset

import scala.collection.JavaConversions._
import scala.collection.breakOut
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration.Duration

import akka.actor.ActorRef
import com.sun.source.tree.{ Scope, IdentifierTree, MemberSelectTree, Tree }
import com.sun.source.util.TreePath
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.{ Element, ExecutableElement, PackageElement, TypeElement, VariableElement }
import javax.lang.model.util.ElementFilter
import org.ensime.api._
import org.ensime.core.CompletionUtil
import org.ensime.util.file._

trait JavaCompletion { this: JavaCompiler =>

  import CompletionUtil._

  protected def scopeForPoint(file: SourceFileInfo, offset: Int): Option[(Compilation, Scope)]
  protected def pathToPoint(file: SourceFileInfo, offset: Int): Option[(Compilation, TreePath)]
  protected def indexer: ActorRef

  def completionsAt(info: SourceFileInfo, offset: Int, maxResultsArg: Int, caseSens: Boolean): CompletionInfoList = {
    val maxResults = if (maxResultsArg == 0) Int.MaxValue else maxResultsArg
    val s = contentsAsString(info, DefaultCharset)

    val preceding = s.slice(Math.max(0, offset - 100), offset)

    val defaultPrefix = JavaIdentRegexp.findFirstMatchIn(preceding) match {
      case Some(m) => m.group(1)
      case _ => ""
    }

    val constructing = ConstructingRegexp.findFirstMatchIn(preceding).isDefined

    val indexAfterTarget = Math.max(0, offset - defaultPrefix.length - 1)

    val precedingChar = s(indexAfterTarget)

    val isMemberAccess = precedingChar == '.'

    val candidates: List[CompletionInfo] = (if (ImportSubtypeRegexp.findFirstMatchIn(preceding).isDefined) {
      // Erase the trailing partial subtype (it breaks type resolution).
      val patched = s.substring(0, indexAfterTarget) + " " + s.substring(indexAfterTarget + defaultPrefix.length + 1);
      (pathToPoint(SourceFileInfo(info.file, Some(patched), None), indexAfterTarget - 1) map {
        case (c: Compilation, path: TreePath) => {
          memberCandidates(c, path.getLeaf, defaultPrefix, true, caseSens)
        }
      })
    } else if (ImportRegexp.findFirstMatchIn(preceding).isDefined) {
      (pathToPoint(info, indexAfterTarget) flatMap {
        case (c: Compilation, path: TreePath) => {
          getEnclosingMemberSelectTree(path).map { m =>
            packageMemberCandidates(c, m, defaultPrefix, caseSens)
          }
        }
      })
    } else if (isMemberAccess) {
      // Erase the trailing partial member (it breaks type resolution).
      val patched = s.substring(0, indexAfterTarget) + ".wait()" + s.substring(indexAfterTarget + defaultPrefix.length + 1);
      (pathToPoint(SourceFileInfo(info.file, Some(patched), None), indexAfterTarget + 1) flatMap {
        case (c: Compilation, path: TreePath) => {
          getEnclosingMemberSelectTree(path).map { m =>
            memberCandidates(c, m.getExpression(), defaultPrefix, false, caseSens)
          }
        }
      })
    } else {

      // Kick off an index search if the name looks like a type.
      val typeSearch = if (TypeNameRegex.findFirstMatchIn(defaultPrefix).isDefined) {
        Some(fetchTypeSearchCompletions(defaultPrefix, maxResults, indexer))
      } else None

      (scopeForPoint(info, indexAfterTarget) map {
        case (c: Compilation, s: Scope) => {
          scopeMemberCandidates(c, s, defaultPrefix, caseSens, constructing)
        }
      }) map { scopeCandidates =>
        val typeSearchResult = typeSearch.flatMap(Await.result(_, Duration.Inf)).getOrElse(List())
        scopeCandidates ++ typeSearchResult
      }

    }).getOrElse(List())
    CompletionInfoList(defaultPrefix, candidates.sortWith({ (c1, c2) =>
      c1.relevance > c2.relevance ||
        (c1.relevance == c2.relevance &&
          c1.name.length < c2.name.length)
    }).take(maxResults))
  }

  private def getEnclosingMemberSelectTree(path: TreePath): Option[MemberSelectTree] = {
    var p = path
    while (p != null) {
      p.getLeaf match {
        case m: MemberSelectTree => return Some(m)
        case _ => {}
      }
      p = p.getParentPath
    }
    None
  }

  private def selectedPackageName(m: MemberSelectTree): String = {
    val name = m.getIdentifier.toString
    m.getExpression match {
      case m: MemberSelectTree => selectedPackageName(m) + "." + name
      case i: IdentifierTree => i.getName.toString() + "." + name
      case _ => name
    }
  }

  private def packageMemberCandidates(
    compilation: Compilation,
    select: MemberSelectTree,
    prefix: String,
    caseSense: Boolean
  ): List[CompletionInfo] = {
    val pkg = selectedPackageName(select)
    val candidates = (Option(compilation.elements.getPackageElement(pkg)) map { p: PackageElement =>
      p.getEnclosedElements().flatMap { e => filterElement(compilation, e, prefix, caseSense, true, false) }
    }).getOrElse(List())
    candidates.toList
  }

  private def filterElement(c: Compilation, e: Element, prefix: String, caseSense: Boolean,
    typesOnly: Boolean, constructors: Boolean, baseRelevance: Int = 0): List[CompletionInfo] = {
    val s = e.getSimpleName.toString

    // reward case case-sensitive matches
    val relevance = if (s.startsWith(prefix)) baseRelevance + 50 else baseRelevance

    if (matchesPrefix(s, prefix, matchEntire = false, caseSens = caseSense) && !s.contains("$") && !s.contains("<init>")) {
      e match {
        case e: ExecutableElement if !typesOnly => List(methodInfo(e, relevance + 5))
        case e: VariableElement if !typesOnly => List(fieldInfo(e, relevance + 10))
        case e: TypeElement => if (constructors) constructorInfos(c, e, relevance + 5) else List(typeInfo(e, relevance))
        case _ => List()
      }
    } else List()
  }

  private def scopeMemberCandidates(
    compilation: Compilation,
    scope: Scope,
    prefix: String,
    caseSense: Boolean,
    constructing: Boolean
  ): List[CompletionInfo] = {
    var candidates = ArrayBuffer[CompletionInfo]()

    // Note Scope#getLocalElements does not include fields / members of
    // enclosing classes. Need to add those manually.
    //
    def addTypeMembers(tel: TypeElement, relevance: Int): Unit = {
      for (el <- compilation.elements.getAllMembers(tel)) {
        for (info <- filterElement(compilation, el, prefix, caseSense, false, constructing, relevance)) {
          candidates += info
        }
      }
    }

    var relevance = 0
    for (tel <- Option(scope.getEnclosingClass())) {
      addTypeMembers(tel, relevance)
      var t = tel.getEnclosingElement()
      while (t != null) {
        t match {
          case tel: TypeElement => addTypeMembers(tel, relevance)
          case _ =>
        }
        t = t.getEnclosingElement()
        relevance -= 10
      }
    }

    relevance = 0
    var s = scope
    while (s != null) {
      for (el <- s.getLocalElements()) {
        for (info <- filterElement(compilation, el, prefix, caseSense, false, constructing, relevance)) {
          candidates += info
        }
      }
      s = s.getEnclosingScope()
      relevance -= 10
    }
    candidates.toList
  }

  private def memberCandidates(
    c: Compilation,
    target: Tree,
    prefix: String,
    importing: Boolean,
    caseSense: Boolean
  ): List[CompletionInfo] = {

    typeElement(c, target).toList.flatMap {

      case tel: TypeElement =>

        val path = c.trees.getPath(c.compilationUnit, target)
        val scope = c.trees.getScope(path)

        val isAccessible: Element => Boolean = c.trees
          .isAccessible(scope, _, c.types.getDeclaredType(tel))

        c.elements.getAllMembers(tel).filter(isAccessible).flatMap { el =>
          filterElement(c, el, prefix, caseSense, importing, false)
        }(breakOut)

      case e =>
        log.warn("Unrecognized type element " + e)
        List.empty
    }
  }

  private def methodInfo(e: ExecutableElement, relevance: Int): CompletionInfo =
    CompletionInfo(
      Some(methodToTypeInfo(e)),
      e.getSimpleName.toString,
      relevance, None
    )

  private def fieldInfo(e: VariableElement, relevance: Int): CompletionInfo = {
    val s = e.getSimpleName.toString
    CompletionInfo(
      None, s, relevance, None
    )
  }

  private def typeInfo(e: TypeElement, relevance: Int): CompletionInfo = {
    val s = e.getSimpleName.toString
    CompletionInfo(
      None, s, relevance, None
    )
  }

  private def constructorInfos(compilation: Compilation, e: TypeElement, relevance: Int): List[CompletionInfo] = {
    val s = e.getSimpleName.toString
    ElementFilter.constructorsIn(compilation.elements.getAllMembers(e)).map(methodInfo(_, relevance)).map { m =>
      m.copy(name = s)
    }.toList
  }

  private def localTypeName(tm: TypeMirror) = {
    val s = tm.toString
    val (front, back) = s.split("\\.").partition { s => s.forall(Character.isLowerCase) }
    if (back.isEmpty) s else back.mkString(".")
  }

  private def contentsAsString(sf: SourceFileInfo, charset: Charset) = sf match {
    case SourceFileInfo(f, None, None) => f.readString()
    case SourceFileInfo(f, Some(contents), None) => contents
    case SourceFileInfo(f, None, Some(contentsIn)) => contentsIn.readString()
  }

}
