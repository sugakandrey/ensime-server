// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.tools.refactoring.implementations.OrganizeImports

trait ScalaRefactoringBackCompat {

  def organizeImportOptions(refactoring: OrganizeImports) = {
    import refactoring.oiWorker.participants._
    List(
      SortImports,
      SortImportSelectors,
      RemoveDuplicates
    )
  }

}
