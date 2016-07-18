// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import scala.concurrent._
import scala.concurrent.duration._
import org.ensime.fixture._
import org.ensime.indexer.graph._
import org.ensime.util.EnsimeSpec
import org.ensime.util.file._
import org.scalatest.Matchers._
import org.scalatest.matchers.{ BeMatcher, MatchResult }

class SearchServiceSpec extends EnsimeSpec
    with SharedTestKitFixture
    with SharedSearchServiceFixture {

  def original = EnsimeConfigFixture.SimpleTestProject

  import SearchServiceTestUtils._

  "search refreshing" should "parse all files on a pristine structure" in {
    withSearchService { implicit service =>
      val (deleted, indexed) = refresh()
      deleted shouldBe 0
      indexed should be > 0
    }
  }

  it should "not refresh files that have not changed" in {
    withSearchService { implicit service =>
      refresh() shouldBe ((0, 0))
    }
  }

  it should "refresh files that have 'changed'" in {
    withSearchService { (config, service) =>
      implicit val s = service
      val now = System.currentTimeMillis()
      for {
        m <- config.modules.values
        r <- m.targets ++ m.testTargets
        f <- r.tree
      } {
        // simulate a full recompile
        f.setLastModified(now)
      }

      val (deleted, indexed) = refresh()
      deleted should be > 0
      indexed should be > 0
    }
  }

  it should "remove classfiles that have been deleted" in {
    withSearchService { (config, service) =>
      implicit val s = service
      val classfile = config.subprojects.head.targets.head / "org/example/Foo.class"

      classfile shouldBe 'exists

      classfile.delete()
      refresh() shouldBe ((1, 0))
      searchExpectEmpty("org.example.Foo")
    }
  }

  "class searching" should "return results from J2SE" in withSearchService { implicit service =>
    searchesClasses(
      "java.lang.String",
      "String", "string",
      "j.l.str", "j l str"
    )
  }

  it should "return results from dependencies" in withSearchService { implicit service =>
    searchesClasses(
      "org.scalatest.FunSuite",
      "FunSuite", "funsuite", "funsu",
      "o s Fun"
    )
  }

  it should "return results from the project" in withSearchService { implicit service =>
    searchesClasses(
      "org.example.Bloo",
      "o e bloo"
    )

    searchesClasses(
      "org.example.Blue$",
      "o e blue"
    )

    searchesClasses(
      "org.example.CaseClassWithCamelCaseName",
      "CaseClassWith", "caseclasswith",
      "o e Case", "o.e.caseclasswith",
      "CCWC" // <= CamelCaseAwesomeNess
    )
  }

  it should "return results from package objects" in withSearchService { implicit service =>
    searchClasses(
      "org.example.package$Blip$",
      "Blip"
    )

    searchClasses(
      "org.example.package$Blop",
      "Blop"
    )
  }

  "class and method searching" should "return results from classes" in {
    withSearchService { implicit service =>
      searchesClasses(
        "java.lang.String",
        "String", "string",
        "j.l.str", "j l str"
      )
    }
  }

  it should "return results from static fields" in withSearchService { implicit service =>
    searchesEmpty(
      "CASE_INSENSITIVE", "case_insensitive",
      "case_"
    )
  }

  it should "not return results from instance fields" in withSearchService { implicit service =>
    searchesEmpty(
      "java.awt.Point.x"
    )
  }

  it should "return results from static methods" in withSearchService { implicit service =>
    searchesMethods(
      "java.lang.Runtime.addShutdownHook(Ljava/lang/Thread;)V",
      "addShutdownHook"
    )
  }

  it should "return results from instance methods" in withSearchService { implicit service =>
    searchesMethods(
      "java.lang.Runtime.availableProcessors()I",
      "availableProcessors", "availableP"
    )
  }

  it should "not prioritise noisy inner classes" in withSearchService { implicit service =>
    def nonTrailingDollarSigns(fqn: String): Int = fqn.count(_ == '$') - (if (fqn.endsWith("$")) 1 else 0)
    def isSorted(hits: Seq[String]): Boolean =
      hits.sliding(2).map {
        case List(x, y) => nonTrailingDollarSigns(x) <= nonTrailingDollarSigns(y)
      }.forall(identity)

    val sorted = new BeMatcher[Seq[String]] {
      override def apply(left: Seq[String]): MatchResult =
        MatchResult(
          isSorted(left),
          s"Elements of $left were not sorted by the amount of non trailing $$'s. ${left.map(nonTrailingDollarSigns)}",
          s"Elements of $left were sorted by the amount of non trailing $$'s"
        )
    }

    val bazHits = service.searchClasses("Baz", 10).map(_.fqn)
    bazHits should contain theSameElementsAs (Seq(
      "org.example2.Baz",
      "org.example2.Baz$Wibble$baz",
      "org.example2.Baz$Wibble$baz$",
      "org.example2.Baz$Wibble$",
      "org.example2.Baz$",
      "org.example2.Baz$Wibble"
    ))
    bazHits should be(sorted)

    val matchersHits = service.searchClasses("Matchers", 25).map(_.fqn)
    matchersHits.take(2) should contain theSameElementsAs Seq(
      "org.scalatest.Matchers",
      "org.scalatest.Matchers$"
    )
    matchersHits should be(sorted)

    val regexHits = service.searchClasses("Regex", 10).map(_.fqn)
    regexHits.take(2) should contain theSameElementsAs Seq(
      "scala.util.matching.Regex",
      "scala.util.matching.Regex$"
    )
    regexHits should be(sorted)
  }

  it should "return user created classes first" in withSearchService { implicit service =>
    val hits = service.searchClasses("File", 10).map(_.fqn)
    hits.head should startWith("org.boost.File")

    val hits2 = service.searchClasses("Function1", 25).map(_.fqn)
    hits2.head should startWith("org.boost.Function1")
  }

  it should "return user methods first" in withSearchService { implicit service =>
    val hits = service.searchClassesMethods("toString" :: Nil, 10).map(_.fqn)
    all(hits) should startWith regex ("org.example|org.boost")
  }

  "exact searches" should "find type aliases" in withSearchService { implicit service =>
    service.findUnique("org.scalatest.fixture.ConfigMapFixture$FixtureParam") shouldBe defined
  }

  "class hierarchy viewer" should "find all classes implementing a trait" in withSearchService { implicit service =>
    val someTrait = "org.hierarchy.SomeTrait"
    val implementingClasses = getClassHierarchy(someTrait, Hierarchy.Subtypes)
    inside(implementingClasses) {
      case TypeHierarchy(classDef, refs) =>
        classDef.fqn should ===(someTrait)
        inside(refs) {
          case Seq(ref1, ref2) =>
            inside(ref1) {
              case TypeHierarchy(aClass, Seq(subclass)) =>
                aClass.fqn should ===("org.hierarchy.ExtendsTrait")
                inside(subclass) {
                  case cdef: ClassDef => cdef.fqn should ===("org.hierarchy.Subclass")
                }
            }
            inside(ref2) {
              case cdef: ClassDef => cdef.fqn should ===("org.hierarchy.ExtendsTraitToo")
            }
        }
    }
  }

  it should "find all superclasses of a class" in withSearchService { implicit service =>
    val hierarchy = getClassHierarchy("org.hierarchy.Qux", Hierarchy.Supertypes)
    hierarchyToSet(hierarchy).map(_.fqn) should contain theSameElementsAs Set(
      "org.hierarchy.Qux",
      "scala.math.Ordered",
      "org.hierarchy.Bar",
      "org.hierarchy.NotBaz",
      "java.lang.Runnable",
      "java.lang.Comparable",
      "java.lang.Object"
    )
  }
}

object SearchServiceTestUtils {

  def refresh()(implicit service: SearchService): (Int, Int) =
    Await.result(service.refresh(), Duration.Inf)

  def searchClasses(expect: String, query: String)(implicit service: SearchService) = {
    val max = 10
    val info = s"'$query' expected '$expect'"
    val results = service.searchClasses(query, max)

    withClue(s"${results.size} $info")(results.size should be <= max)
    withClue(s"$info but was empty")(results should not be empty)
    // when we improve the search quality, we could
    // make this really look only at #1
    val got = results.map(_.fqn)
    withClue(s"$info got '$got'")(got should contain(expect))
    results
  }

  def searchesClasses(expect: String, queries: String*)(implicit service: SearchService) =
    (expect :: queries.toList).foreach(searchClasses(expect, _))

  def searchClassesAndMethods(expect: String, query: String)(implicit service: SearchService) = {
    val max = 10
    val info = s"'$query' expected '$expect')"
    val results = service.searchClassesMethods(List(query), max)
    withClue(s"${results.size} $info")(results.size should be <= max)
    withClue(s"$info but was empty")(results should not be empty)
    // when we improve the search quality, we could
    // make this really look only at #1
    val got = results.map(_.fqn)
    withClue(s"$info got '$got'")(got should contain(expect))
    results
  }

  def searchExpectEmpty(query: String)(implicit service: SearchService) = {
    val max = 1
    val results = service.searchClassesMethods(List(query), max)
    withClue("expected empty results from %s".format(query))(results shouldBe empty)
    results
  }

  def searchesEmpty(queries: String*)(implicit service: SearchService) =
    queries.toList.foreach(searchExpectEmpty)

  // doesn't assert that expect finds itself because the lucene query
  // syntax conflicts with the characters in method FQNs
  def searchesMethods(expect: String, queries: String*)(implicit service: SearchService) =
    (queries.toList).foreach(searchClassesAndMethods(expect, _))

  def getClassHierarchy(fqn: String, hierarchyType: Hierarchy.Direction)(implicit service: SearchService): Hierarchy = {
    val hierarchy = Await.result(service.getClassHierarchy(fqn, hierarchyType), Duration.Inf)
    withClue(s"No class hierarchy found for fqn = $fqn")(hierarchy shouldBe defined)
    hierarchy.get
  }

  def hierarchyToSet(hierarchy: Hierarchy): Set[ClassDef] = hierarchy match {
    case cdef: ClassDef => Set(cdef)
    case TypeHierarchy(cdef, typeRefs) => Set(cdef) ++ typeRefs.flatMap(hierarchyToSet)
  }
}
