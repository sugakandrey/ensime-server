package org.ensime.core

import scala.tools.refactoring.implementations.OrganizeImports

trait ScalaRefactoringBackCompat {

  def organizeImportOptions(refactoring: OrganizeImports) = {
    import refactoring._
    List(
      SortImports,
      SimplifyWildcards,
      SortImportSelectors,
      RemoveDuplicates
    )
  }

}
