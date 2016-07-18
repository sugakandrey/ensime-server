// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.swanky

import org.ensime.sexp._
import org.ensime.api._
import org.ensime.util.{ EnsimeSpec, EscapingStringInterpolation }
import org.scalactic.source.Position

class SwankyFormatsSpec extends EnsimeSpec with EnsimeTestData {
  import SwankyFormats._

  import EscapingStringInterpolation._

  // copied from s-express:test to avoid a test->test dependency
  def assertFormat[T](start: T, expect: Sexp)(implicit f: SexpFormat[T], p: Position): Unit = {
    val sexp = start.toSexp
    val converted = sexp == expect // reduces noise in scalatest reporting
    assert(converted, s"\n${sexp.compactPrint}\nwas not\n${expect.compactPrint}")
    expect.convertTo[T] should be(start)
  }

  def roundtrip(value: RpcRequest, via: String)(implicit p: Position): Unit = {
    val enveloped = RpcRequestEnvelope(value, -1)
    assertFormat(enveloped, s"""(:req $via :call-id -1)""".parseSexp)
  }

  def roundtrip(value: EnsimeServerMessage, via: String)(implicit p: Position): Unit = {
    val enveloped = RpcResponseEnvelope(None, value)
    assertFormat(enveloped, s"""(:payload $via)""".parseSexp)
  }

  "SWANK Formats" should "roundtrip startup messages" in {
    roundtrip(
      ConnectionInfoReq: RpcRequest,
      ":ensime-api-connection-info-req"
    )
  }

  it should "roundtrip RpcSearchRequests" in {
    roundtrip(
      PublicSymbolSearchReq(List("foo", "bar"), 10): RpcRequest,
      """(:ensime-api-public-symbol-search-req (:keywords ("foo" "bar") :max-results 10))"""
    )

    roundtrip(
      ImportSuggestionsReq(Left(file1), 1, List("foo", "bar"), 10): RpcRequest,
      s"""(:ensime-api-import-suggestions-req (:file "$file1" :point 1 :names ("foo" "bar") :max-results 10))"""
    )
  }

  it should "roundtrip RpcAnalyserRequests" in {
    roundtrip(
      RemoveFileReq(file1): RpcRequest,
      s"""(:ensime-api-remove-file-req (:file "$file1"))"""
    )

    roundtrip(
      TypecheckFileReq(sourceFileInfo): RpcRequest,
      s"""(:ensime-api-typecheck-file-req (:file-info (:file "$file1" :contents "{/* code here */}" :contents-in "$file2")))"""
    )

    roundtrip(
      TypecheckFilesReq(List(Left(file1), Left(file2))): RpcRequest,
      s"""(:ensime-api-typecheck-files-req (:files ("$file1" "$file2")))"""
    )

    roundtrip(
      TypecheckFilesReq(List(Right(SourceFileInfo(file1)), Right(SourceFileInfo(file2, Some("xxx"), None)))): RpcRequest,
      s"""(:ensime-api-typecheck-files-req (:files ((:file "$file1") (:file "$file2" :contents "xxx"))))"""
    )

    roundtrip(
      UnloadAllReq: RpcRequest,
      """:ensime-api-unload-all-req"""
    )

    roundtrip(
      TypecheckAllReq: RpcRequest,
      """:ensime-api-typecheck-all-req"""
    )

    roundtrip(
      FormatSourceReq(List(file1, file2)): RpcRequest,
      s"""(:ensime-api-format-source-req (:files ("$file1" "$file2")))"""
    )

    roundtrip(
      FormatOneSourceReq(sourceFileInfo): RpcRequest,
      s"""(:ensime-api-format-one-source-req (:file (:file "$file1" :contents "{/* code here */}" :contents-in "$file2")))"""
    )

    roundtrip(
      DocUriAtPointReq(Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""(:ensime-api-doc-uri-at-point-req (:file "$file1" :point (:from 1 :to 10)))"""
    )

    roundtrip(
      DocUriAtPointReq(Right(SourceFileInfo(file1, None, Some(file2))), OffsetRange(1, 10)): RpcRequest,
      s"""(:ensime-api-doc-uri-at-point-req (:file (:file "$file1" :contents-in "$file2") :point (:from 1 :to 10)))"""
    )

    roundtrip(
      DocUriForSymbolReq("foo.bar", Some("Baz"), None): RpcRequest,
      s"""(:ensime-api-doc-uri-for-symbol-req (:type-full-name "foo.bar" :member-name "Baz"))"""
    )

    roundtrip(
      CompletionsReq(sourceFileInfo, 10, 100, true, false): RpcRequest,
      s"""(:ensime-api-completions-req (:file-info (:file "$file1" :contents "{/* code here */}" :contents-in "$file2") :point 10 :max-results 100 :case-sens t))"""
    )

    roundtrip(
      PackageMemberCompletionReq("foo", "bar"): RpcRequest,
      """(:ensime-api-package-member-completion-req (:path "foo" :prefix "bar"))"""
    )

    roundtrip(
      UsesOfSymbolAtPointReq(Left(file1), 100): RpcRequest,
      s"""(:ensime-api-uses-of-symbol-at-point-req (:file "$file1" :point 100))"""
    )

    roundtrip(
      TypeByNameReq("foo.bar"): RpcRequest,
      s"""(:ensime-api-type-by-name-req (:name "foo.bar"))"""
    )

    roundtrip(
      TypeByNameAtPointReq("foo.bar", Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""(:ensime-api-type-by-name-at-point-req (:name "foo.bar" :file "$file1" :range (:from 1 :to 10)))"""
    )

    roundtrip(
      TypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""(:ensime-api-type-at-point-req (:file "$file1" :range (:from 1 :to 100)))"""
    )

    roundtrip(
      InspectTypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""(:ensime-api-inspect-type-at-point-req (:file "$file1" :range (:from 1 :to 100)))"""
    )

    roundtrip(
      InspectTypeByNameReq("foo.Bar"): RpcRequest,
      s"""(:ensime-api-inspect-type-by-name-req (:name "foo.Bar"))"""
    )

    roundtrip(
      SymbolAtPointReq(Left(file1), 101): RpcRequest,
      s"""(:ensime-api-symbol-at-point-req (:file "$file1" :point 101))"""
    )

    roundtrip(
      SymbolByNameReq("foo.Bar", Some("baz"), None): RpcRequest,
      s"""(:ensime-api-symbol-by-name-req (:type-full-name "foo.Bar" :member-name "baz"))"""
    )

    roundtrip(
      InspectPackageByPathReq("foo.bar"): RpcRequest,
      s"""(:ensime-api-inspect-package-by-path-req (:path "foo.bar"))"""
    )

    roundtrip(
      RefactorReq(1, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest,
      s"""(:ensime-api-refactor-req (:proc-id 1 :params (:ensime-api-rename-refactor-desc (:new-name "bar" :file "$file1" :start 1 :end 100))))"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Left(file1), 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""(:ensime-api-symbol-designations-req (:file "$file1" :start 1 :end 100 :requested-types (:ensime-api-object-symbol :ensime-api-val-symbol)))"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Right(SourceFileInfo(file1, None, None)), 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""(:ensime-api-symbol-designations-req (:file (:file "$file1") :start 1 :end 100 :requested-types (:ensime-api-object-symbol :ensime-api-val-symbol)))"""
    )

    roundtrip(
      ExpandSelectionReq(file1, 100, 200): RpcRequest,
      s"""(:ensime-api-expand-selection-req (:file "$file1" :start 100 :end 200))"""
    )

    roundtrip(
      ImplicitInfoReq(Left(file1), OffsetRange(0, 123)),
      s"""(:ensime-api-implicit-info-req (:file "$file1" :range (:from 0 :to 123)))"""
    )

    roundtrip(
      StructureViewReq(sourceFileInfo): RpcRequest,
      s"""(:ensime-api-structure-view-req (:file-info (:file "$file1" :contents "{/* code here */}" :contents-in "$file2")))"""
    )

    roundtrip(
      AstAtPointReq(sourceFileInfo, OffsetRange(1, 100)): RpcRequest,
      s"""(:ensime-api-ast-at-point-req (:file (:file "$file1" :contents "{/* code here */}" :contents-in "$file2") :offset (:from 1 :to 100)))"""
    )
  }

  it should "roundtrip RpcDebugRequests" in {
    roundtrip(
      DebugActiveVmReq: RpcRequest,
      """:ensime-api-debug-active-vm-req"""
    )

    roundtrip(
      DebugAttachReq("mylovelyhorse", "13"): RpcRequest,
      """(:ensime-api-debug-attach-req (:hostname "mylovelyhorse" :port "13"))"""
    )

    roundtrip(
      DebugStopReq: RpcRequest,
      """:ensime-api-debug-stop-req"""
    )

    roundtrip(
      DebugSetBreakReq(file1, 13): RpcRequest,
      s"""(:ensime-api-debug-set-break-req (:file "$file1" :line 13))"""
    )

    roundtrip(
      DebugClearBreakReq(file1, 13): RpcRequest,
      s"""(:ensime-api-debug-clear-break-req (:file "$file1" :line 13))"""
    )

    roundtrip(
      DebugClearAllBreaksReq: RpcRequest,
      s""":ensime-api-debug-clear-all-breaks-req"""
    )

    roundtrip(
      DebugListBreakpointsReq: RpcRequest,
      s""":ensime-api-debug-list-breakpoints-req"""
    )

    roundtrip(
      DebugRunReq: RpcRequest,
      s""":ensime-api-debug-run-req"""
    )

    roundtrip(
      DebugContinueReq(dtid): RpcRequest,
      s"""(:ensime-api-debug-continue-req (:thread-id 13))"""
    )

    roundtrip(
      DebugStepReq(dtid): RpcRequest,
      s"""(:ensime-api-debug-step-req (:thread-id 13))"""
    )

    roundtrip(
      DebugNextReq(dtid): RpcRequest,
      s"""(:ensime-api-debug-next-req (:thread-id 13))"""
    )

    roundtrip(
      DebugStepOutReq(dtid): RpcRequest,
      s"""(:ensime-api-debug-step-out-req (:thread-id 13))"""
    )

    roundtrip(
      DebugLocateNameReq(dtid, "foo"): RpcRequest,
      s"""(:ensime-api-debug-locate-name-req (:thread-id 13 :name "foo"))"""
    )

    roundtrip(
      DebugValueReq(debugLocationArray): RpcRequest,
      s"""(:ensime-api-debug-value-req (:loc (:ensime-api-debug-array-element (:object-id 13 :index 14))))"""
    )

    roundtrip(
      DebugToStringReq(dtid, debugLocationArray): RpcRequest,
      s"""(:ensime-api-debug-to-string-req (:thread-id 13 :loc (:ensime-api-debug-array-element (:object-id 13 :index 14))))"""
    )

    roundtrip(
      DebugSetValueReq(debugLocationArray, "bar"): RpcRequest,
      s"""(:ensime-api-debug-set-value-req (:loc (:ensime-api-debug-array-element (:object-id 13 :index 14)) :new-value "bar"))"""
    )

    roundtrip(
      DebugBacktraceReq(dtid, 100, 200): RpcRequest,
      s"""(:ensime-api-debug-backtrace-req (:thread-id 13 :index 100 :count 200))"""
    )

  }

  it should "roundtrip EnsimeGeneralEvent as EnsimeEvent" in {
    roundtrip(
      SendBackgroundMessageEvent("ABCDEF", 1): EnsimeEvent,
      """(:ensime-api-send-background-message-event (:detail "ABCDEF" :code 1))"""
    )

    roundtrip(
      AnalyzerReadyEvent: EnsimeEvent,
      ":ensime-api-analyzer-ready-event"
    )

    roundtrip(
      FullTypeCheckCompleteEvent: EnsimeEvent,
      ":ensime-api-full-type-check-complete-event"
    )

    roundtrip(
      IndexerReadyEvent: EnsimeEvent,
      ":ensime-api-indexer-ready-event"
    )

    roundtrip(
      NewScalaNotesEvent(
        isFull = false,
        List(Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
      ): EnsimeEvent,
      """(:ensime-api-new-scala-notes-event (:notes ((:file "foo.scala" :msg "testMsg" :severity :ensime-api-note-warn :beg 50 :end 55 :line 77 :col 5))))"""
    )

    roundtrip(
      ClearAllScalaNotesEvent: EnsimeEvent,
      ":ensime-api-clear-all-scala-notes-event"
    )
  }

  it should "roundtrip DebugEvent as EnsimeEvent" in {
    roundtrip(
      DebugOutputEvent("XXX"): EnsimeEvent,
      """(:ensime-api-debug-output-event (:body "XXX"))"""
    )

    roundtrip(
      DebugStepEvent(DebugThreadId(207), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      s"""(:ensime-api-debug-step-event (:thread-id 207 :thread-name "threadNameStr" :file "$file1" :line 57))"""
    )

    roundtrip(
      DebugBreakEvent(DebugThreadId(209), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      s"""(:ensime-api-debug-break-event (:thread-id 209 :thread-name "threadNameStr" :file "$file1" :line 57))"""
    )

    roundtrip(
      DebugVmStartEvent: EnsimeEvent,
      """:ensime-api-debug-vm-start-event"""
    )

    roundtrip(
      DebugVmDisconnectEvent: EnsimeEvent,
      """:ensime-api-debug-vm-disconnect-event"""
    )

    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", Some(sourcePos1.file), Some(sourcePos1.line)): EnsimeEvent,
      s"""(:ensime-api-debug-exception-event (:exception 33 :thread-id 13 :thread-name "threadNameStr" :file "$file1" :line 57))"""
    )

    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", None, None): EnsimeEvent,
      """(:ensime-api-debug-exception-event (:exception 33 :thread-id 13 :thread-name "threadNameStr"))"""
    )

    roundtrip(
      DebugThreadStartEvent(dtid): EnsimeEvent,
      """(:ensime-api-debug-thread-start-event (:thread-id 13))"""
    )

    roundtrip(
      DebugThreadDeathEvent(dtid): EnsimeEvent,
      """(:ensime-api-debug-thread-death-event (:thread-id 13))"""
    )
  }

  it should "roundtrip DebugLocation" in {
    roundtrip(
      DebugObjectReference(57L): DebugLocation,
      """(:ensime-api-debug-object-reference (:object-id 57))"""
    )

    roundtrip(
      DebugArrayElement(DebugObjectId(58L), 2): DebugLocation,
      """(:ensime-api-debug-array-element (:object-id 58 :index 2))"""
    )

    roundtrip(
      DebugObjectField(DebugObjectId(58L), "fieldName"): DebugLocation,
      """(:ensime-api-debug-object-field (:object-id 58 :field "fieldName"))"""
    )

    roundtrip(
      DebugStackSlot(DebugThreadId(27), 12, 23): DebugLocation,
      """(:ensime-api-debug-stack-slot (:thread-id 27 :frame 12 :offset 23))"""
    )
  }

  it should "roundtrip DebugValue" in {
    roundtrip(
      DebugPrimitiveValue("summaryStr", "typeNameStr"): DebugValue,
      """(:ensime-api-debug-primitive-value (:summary "summaryStr" :type-name "typeNameStr"))"""
    )

    roundtrip(
      DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """(:ensime-api-debug-string-instance (:summary "summaryStr" :fields ((:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")) :type-name "typeNameStr" :object-id 5))"""
    )

    roundtrip(
      DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """(:ensime-api-debug-object-instance (:summary "summaryStr" :fields ((:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")) :type-name "typeNameStr" :object-id 5))"""
    )

    roundtrip(
      DebugNullValue("typeNameStr"): DebugValue,
      """(:ensime-api-debug-null-value (:type-name "typeNameStr"))"""
    )

    roundtrip(
      DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L)): DebugValue,
      """(:ensime-api-debug-array-instance (:length 3 :type-name "typeName" :element-type-name "elementType" :object-id 5))"""
    )

    roundtrip(
      debugClassField: DebugClassField,
      """(:ensime-api-debug-class-field (:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr"))"""
    )

    roundtrip(
      debugStackLocal1: DebugStackLocal,
      """(:ensime-api-debug-stack-local (:index 3 :name "name1" :summary "summary1" :type-name "type1"))"""
    )

    roundtrip(
      debugStackFrame: DebugStackFrame,
      s"""(:ensime-api-debug-stack-frame (:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file "$file1" :line 57) :this-object-id 7))"""
    )

    roundtrip(
      DebugBacktrace(List(debugStackFrame), dtid, "thread1"): DebugBacktrace,
      s"""(:ensime-api-debug-backtrace (:frames ((:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file "$file1" :line 57) :this-object-id 7)) :thread-id 13 :thread-name "thread1"))"""
    )

    roundtrip(
      sourcePos1: SourcePosition,
      s"""(:ensime-api-line-source-position (:file "$file1" :line 57))"""
    )

    roundtrip(
      sourcePos2: SourcePosition,
      s"""(:ensime-api-line-source-position (:file "$file1" :line 59))"""
    )

    roundtrip(
      sourcePos3: SourcePosition,
      ":ensime-api-empty-source-position"
    )

    roundtrip(
      sourcePos4: SourcePosition,
      s"""(:ensime-api-offset-source-position (:file "$file1" :offset 456))"""
    )

    roundtrip(
      breakPoint1: Breakpoint,
      s"""(:ensime-api-breakpoint (:file "$file1" :line 57))"""
    )

    roundtrip(
      BreakpointList(List(breakPoint1), List(breakPoint2)): BreakpointList,
      s"""(:ensime-api-breakpoint-list (:active ((:file "$file1" :line 57)) :pending ((:file "$file1" :line 59))))"""
    )

    roundtrip(
      DebugVmSuccess(): DebugVmStatus,
      """(:ensime-api-debug-vm-success (:status "success"))"""
    )

    roundtrip(
      DebugVmError(303, "xxxx"): DebugVmStatus,
      """(:ensime-api-debug-vm-error (:error-code 303 :details "xxxx" :status "error"))"""
    )
  }

  it should "roundtrip various informational types" in {
    roundtrip(
      note1: Note,
      """(:ensime-api-note (:file "file1" :msg "note1" :severity :ensime-api-note-error :beg 23 :end 33 :line 19 :col 8))"""
    )

    roundtrip(
      completionInfo: CompletionInfo,
      """(:ensime-api-completion-info (:type-info (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :name "name" :relevance 90 :to-insert "BAZ"))"""
    )

    roundtrip(
      completionInfo2: CompletionInfo,
      """(:ensime-api-completion-info (:name "name2" :relevance 91))"""
    )

    roundtrip(
      CompletionInfoList("fooBar", List(completionInfo)): CompletionInfoList,
      """(:ensime-api-completion-info-list (:prefix "fooBar" :completions ((:type-info (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :name "name" :relevance 90 :to-insert "BAZ"))))"""
    )

    roundtrip(
      SymbolInfo("name", "localName", None, typeInfo): SymbolInfo,
      """(:ensime-api-symbol-info (:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))))"""
    )

    roundtrip(
      NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method): EntityInfo,
      """(:ensime-api-named-type-member-info (:name "typeX" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :decl-as :ensime-api-method))"""
    )

    roundtrip(
      entityInfo: EntityInfo,
      """(:ensime-api-arrow-type-info (:name "Arrow1" :full-name "example.Arrow1" :result-type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :param-sections ((:params ((:_1 "ABC" :_2 (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))))))))"""
    )

    roundtrip(
      entityInfoTypeParams: EntityInfo,
      s"""(:ensime-api-arrow-type-info (:name "Arrow1" :full-name "example.Arrow1" :result-type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :param-sections ((:params ((:_1 "ABC" :_2 (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")))))) :type-params ((:ensime-api-basic-type-info (:name "A" :decl-as :ensime-api-nil :full-name "example.Arrow1.A")) (:ensime-api-basic-type-info (:name "B" :decl-as :ensime-api-nil :full-name "example.Arrow1.B")))))"""
    )

    roundtrip(
      typeInfo: EntityInfo,
      """(:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))"""
    )

    roundtrip(
      packageInfo: EntityInfo,
      """(:ensime-api-package-info (:name "name" :full-name "fullName"))"""
    )

    roundtrip(
      interfaceInfo: InterfaceInfo,
      """(:ensime-api-interface-info (:type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :via-view "DEF"))"""
    )

    roundtrip(
      TypeInspectInfo(typeInfo, List(interfaceInfo)): TypeInspectInfo,
      """(:ensime-api-type-inspect-info (:type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :interfaces ((:type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :via-view "DEF")) :info-type typeInspect))"""
    )

    roundtrip(
      structureView: StructureView,
      s"""(:ensime-api-structure-view (:view ((:keyword "class" :name "StructureView" :position (:ensime-api-line-source-position (:file "$file1" :line 57))) (:keyword "object" :name "StructureView" :position (:ensime-api-line-source-position (:file "$file1" :line 59)) :members ((:keyword "type" :name "BasicType" :position (:ensime-api-offset-source-position (:file "$file1" :offset 456))))))))"""
    )

    roundtrip(
      astInfo: AstInfo,
      """(:ensime-api-ast-info (:ast "List(Apply(Select(Literal(Constant(1)), TermName(\"$plus\")), List(Literal(Constant(1)))))"))"""
    )
  }

  it should "roundtrip search related responses" in {
    roundtrip(
      SymbolSearchResults(List(methodSearchRes, typeSearchRes)): SymbolSearchResults,
      s"""(:ensime-api-symbol-search-results (:syms ((:ensime-api-method-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-method :pos (:ensime-api-line-source-position (:file "$abd" :line 10)) :owner-name "ownerStr")) (:ensime-api-type-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-trait :pos (:ensime-api-line-source-position (:file "$abd" :line 10)))))))"""
    )

    roundtrip(
      ImportSuggestions(List(List(methodSearchRes, typeSearchRes))): ImportSuggestions,
      s"""(:ensime-api-import-suggestions (:sym-lists (((:ensime-api-method-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-method :pos (:ensime-api-line-source-position (:file "$abd" :line 10)) :owner-name "ownerStr")) (:ensime-api-type-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-trait :pos (:ensime-api-line-source-position (:file "$abd" :line 10))))))))"""
    )

    roundtrip(
      methodSearchRes: SymbolSearchResult,
      s"""(:ensime-api-method-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-method :pos (:ensime-api-line-source-position (:file "$abd" :line 10)) :owner-name "ownerStr"))"""
    )

    roundtrip(
      typeSearchRes: SymbolSearchResult,
      s"""(:ensime-api-type-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-trait :pos (:ensime-api-line-source-position (:file "$abd" :line 10))))"""
    )
  }

  it should "roundtrip ranges and semantic highlighting" in {
    roundtrip(
      ERangePositions(ERangePosition(batchSourceFile, 75, 70, 90) :: Nil),
      """(:ensime-api-e-range-positions (:positions ((:file "/abc" :offset 75 :start 70 :end 90))))"""
    )

    roundtrip(
      FileRange("/abc", 7, 9): FileRange,
      """(:ensime-api-file-range (:file "/abc" :start 7 :end 9))"""
    )

    roundtrip(
      SymbolDesignations(
        symFile, List(
        SymbolDesignation(7, 9, VarFieldSymbol),
        SymbolDesignation(11, 22, ClassSymbol)
      )
      ): SymbolDesignations,
      s"""(:ensime-api-symbol-designations (:file "$symFile" :syms ((:start 7 :end 9 :sym-type :ensime-api-var-field-symbol) (:start 11 :end 22 :sym-type :ensime-api-class-symbol))))"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitConversionInfo(5, 6, symbolInfo))): ImplicitInfos,
      """(:ensime-api-implicit-infos (:infos ((:ensime-api-implicit-conversion-info (:start 5 :end 6 :fun (:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))))))))"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitParamInfo(5, 6, symbolInfo, List(symbolInfo, symbolInfo), true))): ImplicitInfos,
      s"""(:ensime-api-implicit-infos (:infos ((:ensime-api-implicit-param-info (:start 5 :end 6 :fun (:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))) :params ((:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))) (:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")))) :fun-is-implicit t)))))"""
    )
  }

  it should "roundtrip refactoring messages" in {
    roundtrip(
      RefactorFailure(7, "message"): RefactorFailure,
      """(:ensime-api-refactor-failure (:procedure-id 7 :reason "message" :status failure))"""
    )

    roundtrip(
      refactorDiffEffect: RefactorDiffEffect,
      s"""(:ensime-api-refactor-diff-effect (:procedure-id 9 :refactor-type :ensime-api-add-import :diff "$file2"))"""
    )

  }

  it should "roundtrip legacy raw response types" in {
    roundtrip(
      FalseResponse,
      ":ensime-api-false-response"
    )

    roundtrip(
      TrueResponse,
      ":ensime-api-true-response"
    )

    roundtrip(
      StringResponse("wibble"),
      """(:ensime-api-string-response (:text "wibble"))"""
    )

    roundtrip(
      VoidResponse,
      """:ensime-api-void-response"""
    )

  }

}
