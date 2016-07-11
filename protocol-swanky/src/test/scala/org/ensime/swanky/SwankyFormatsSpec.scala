// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.swanky

import org.ensime.sexp._
import org.ensime.api._
import org.ensime.util.{ EnsimeSpec, EscapingStringInterpolation }

class SwankyFormatsSpec extends EnsimeSpec with EnsimeTestData {
  import SwankyFormats._

  import EscapingStringInterpolation._

  // copied from s-express:test to avoid a test->test dependency
  def assertFormat[T: SexpFormat](start: T, expect: Sexp): Unit = {
    val sexp = start.toSexp
    val converted = sexp == expect // reduces noise in scalatest reporting
    assert(converted, s"\n${sexp.compactPrint}\nwas not\n${expect.compactPrint}")
    expect.convertTo[T] should be(start)
  }

  def roundtrip(value: RpcRequest, via: String): Unit = {
    val enveloped = RpcRequestEnvelope(value, -1)
    assertFormat(enveloped, s"""(:req $via :call-id -1)""".parseSexp)
  }

  def roundtrip(value: EnsimeServerMessage, via: String): Unit = {
    val enveloped = RpcResponseEnvelope(None, value)
    assertFormat(enveloped, s"""(:payload $via)""".parseSexp)
  }

  "SWANK Formats" should "roundtrip startup messages" in {
    roundtrip(
      ConnectionInfoReq: RpcRequest,
      ":connection-info-req"
    )
  }

  it should "roundtrip RpcSearchRequests" in {
    roundtrip(
      PublicSymbolSearchReq(List("foo", "bar"), 10): RpcRequest,
      """(:public-symbol-search-req (:keywords ("foo" "bar") :max-results 10))"""
    )

    roundtrip(
      ImportSuggestionsReq(Left(file1), 1, List("foo", "bar"), 10): RpcRequest,
      s"""(:import-suggestions-req (:file "$file1" :point 1 :names ("foo" "bar") :max-results 10))"""
    )
  }

  it should "roundtrip RpcAnalyserRequests" in {
    roundtrip(
      RemoveFileReq(file1): RpcRequest,
      s"""(:remove-file-req (:file "$file1"))"""
    )

    roundtrip(
      TypecheckFileReq(sourceFileInfo): RpcRequest,
      s"""(:typecheck-file-req (:file-info (:file "$file1" :contents "{/* code here */}" :contents-in "$file2")))"""
    )

    roundtrip(
      TypecheckFilesReq(List(Left(file1), Left(file2))): RpcRequest,
      s"""(:typecheck-files-req (:files ("$file1" "$file2")))"""
    )

    roundtrip(
      TypecheckFilesReq(List(Right(SourceFileInfo(file1)), Right(SourceFileInfo(file2, Some("xxx"), None)))): RpcRequest,
      s"""(:typecheck-files-req (:files ((:file "$file1") (:file "$file2" :contents "xxx"))))"""
    )

    roundtrip(
      UnloadAllReq: RpcRequest,
      """:unload-all-req"""
    )

    roundtrip(
      TypecheckAllReq: RpcRequest,
      """:typecheck-all-req"""
    )

    roundtrip(
      FormatSourceReq(List(file1, file2)): RpcRequest,
      s"""(:format-source-req (:files ("$file1" "$file2")))"""
    )

    roundtrip(
      FormatOneSourceReq(sourceFileInfo): RpcRequest,
      s"""(:format-one-source-req (:file (:file "$file1" :contents "{/* code here */}" :contents-in "$file2")))"""
    )

    roundtrip(
      DocUriAtPointReq(Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""(:doc-uri-at-point-req (:file "$file1" :point (:from 1 :to 10)))"""
    )

    roundtrip(
      DocUriAtPointReq(Right(SourceFileInfo(file1, None, Some(file2))), OffsetRange(1, 10)): RpcRequest,
      s"""(:doc-uri-at-point-req (:file (:file "$file1" :contents-in "$file2") :point (:from 1 :to 10)))"""
    )

    roundtrip(
      DocUriForSymbolReq("foo.bar", Some("Baz"), None): RpcRequest,
      s"""(:doc-uri-for-symbol-req (:type-full-name "foo.bar" :member-name "Baz"))"""
    )

    roundtrip(
      CompletionsReq(sourceFileInfo, 10, 100, true, false): RpcRequest,
      s"""(:completions-req (:file-info (:file "$file1" :contents "{/* code here */}" :contents-in "$file2") :point 10 :max-results 100 :case-sens t))"""
    )

    roundtrip(
      PackageMemberCompletionReq("foo", "bar"): RpcRequest,
      """(:package-member-completion-req (:path "foo" :prefix "bar"))"""
    )

    roundtrip(
      UsesOfSymbolAtPointReq(Left(file1), 100): RpcRequest,
      s"""(:uses-of-symbol-at-point-req (:file "$file1" :point 100))"""
    )

    roundtrip(
      TypeByNameReq("foo.bar"): RpcRequest,
      s"""(:type-by-name-req (:name "foo.bar"))"""
    )

    roundtrip(
      TypeByNameAtPointReq("foo.bar", Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""(:type-by-name-at-point-req (:name "foo.bar" :file "$file1" :range (:from 1 :to 10)))"""
    )

    roundtrip(
      TypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""(:type-at-point-req (:file "$file1" :range (:from 1 :to 100)))"""
    )

    roundtrip(
      InspectTypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""(:inspect-type-at-point-req (:file "$file1" :range (:from 1 :to 100)))"""
    )

    roundtrip(
      InspectTypeByNameReq("foo.Bar"): RpcRequest,
      s"""(:inspect-type-by-name-req (:name "foo.Bar"))"""
    )

    roundtrip(
      SymbolAtPointReq(Left(file1), 101): RpcRequest,
      s"""(:symbol-at-point-req (:file "$file1" :point 101))"""
    )

    roundtrip(
      SymbolByNameReq("foo.Bar", Some("baz"), None): RpcRequest,
      s"""(:symbol-by-name-req (:type-full-name "foo.Bar" :member-name "baz"))"""
    )

    roundtrip(
      InspectPackageByPathReq("foo.bar"): RpcRequest,
      s"""(:inspect-package-by-path-req (:path "foo.bar"))"""
    )

    roundtrip(
      RefactorReq(1, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest,
      s"""(:refactor-req (:proc-id 1 :params (:rename-refactor-desc (:new-name "bar" :file "$file1" :start 1 :end 100))))"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Left(file1), 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""(:symbol-designations-req (:file "$file1" :start 1 :end 100 :requested-types (:object-symbol :val-symbol)))"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Right(SourceFileInfo(file1, None, None)), 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""(:symbol-designations-req (:file (:file "$file1") :start 1 :end 100 :requested-types (:object-symbol :val-symbol)))"""
    )

    roundtrip(
      ExpandSelectionReq(file1, 100, 200): RpcRequest,
      s"""(:expand-selection-req (:file "$file1" :start 100 :end 200))"""
    )

    roundtrip(
      ImplicitInfoReq(Left(file1), OffsetRange(0, 123)),
      s"""(:implicit-info-req (:file "$file1" :range (:from 0 :to 123)))"""
    )

    roundtrip(
      StructureViewReq(sourceFileInfo): RpcRequest,
      s"""(:structure-view-req (:file-info (:file "$file1" :contents "{/* code here */}" :contents-in "$file2")))"""
    )

    roundtrip(
      AstAtPointReq(sourceFileInfo, OffsetRange(1, 100)): RpcRequest,
      s"""(:ast-at-point-req (:file (:file "$file1" :contents "{/* code here */}" :contents-in "$file2") :offset (:from 1 :to 100)))"""
    )
  }

  it should "roundtrip RpcDebugRequests" in {
    roundtrip(
      DebugActiveVmReq: RpcRequest,
      """:debug-active-vm-req"""
    )

    roundtrip(
      DebugAttachReq("mylovelyhorse", "13"): RpcRequest,
      """(:debug-attach-req (:hostname "mylovelyhorse" :port "13"))"""
    )

    roundtrip(
      DebugStopReq: RpcRequest,
      """:debug-stop-req"""
    )

    roundtrip(
      DebugSetBreakReq(file1, 13): RpcRequest,
      s"""(:debug-set-break-req (:file "$file1" :line 13))"""
    )

    roundtrip(
      DebugClearBreakReq(file1, 13): RpcRequest,
      s"""(:debug-clear-break-req (:file "$file1" :line 13))"""
    )

    roundtrip(
      DebugClearAllBreaksReq: RpcRequest,
      s""":debug-clear-all-breaks-req"""
    )

    roundtrip(
      DebugListBreakpointsReq: RpcRequest,
      s""":debug-list-breakpoints-req"""
    )

    roundtrip(
      DebugRunReq: RpcRequest,
      s""":debug-run-req"""
    )

    roundtrip(
      DebugContinueReq(dtid): RpcRequest,
      s"""(:debug-continue-req (:thread-id 13))"""
    )

    roundtrip(
      DebugStepReq(dtid): RpcRequest,
      s"""(:debug-step-req (:thread-id 13))"""
    )

    roundtrip(
      DebugNextReq(dtid): RpcRequest,
      s"""(:debug-next-req (:thread-id 13))"""
    )

    roundtrip(
      DebugStepOutReq(dtid): RpcRequest,
      s"""(:debug-step-out-req (:thread-id 13))"""
    )

    roundtrip(
      DebugLocateNameReq(dtid, "foo"): RpcRequest,
      s"""(:debug-locate-name-req (:thread-id 13 :name "foo"))"""
    )

    roundtrip(
      DebugValueReq(debugLocationArray): RpcRequest,
      s"""(:debug-value-req (:loc (:debug-array-element (:object-id 13 :index 14))))"""
    )

    roundtrip(
      DebugToStringReq(dtid, debugLocationArray): RpcRequest,
      s"""(:debug-to-string-req (:thread-id 13 :loc (:debug-array-element (:object-id 13 :index 14))))"""
    )

    roundtrip(
      DebugSetValueReq(debugLocationArray, "bar"): RpcRequest,
      s"""(:debug-set-value-req (:loc (:debug-array-element (:object-id 13 :index 14)) :new-value "bar"))"""
    )

    roundtrip(
      DebugBacktraceReq(dtid, 100, 200): RpcRequest,
      s"""(:debug-backtrace-req (:thread-id 13 :index 100 :count 200))"""
    )

  }

  it should "roundtrip EnsimeGeneralEvent as EnsimeEvent" in {
    roundtrip(
      SendBackgroundMessageEvent("ABCDEF", 1): EnsimeEvent,
      """(:send-background-message-event (:detail "ABCDEF" :code 1))"""
    )

    roundtrip(
      AnalyzerReadyEvent: EnsimeEvent,
      ":analyzer-ready-event"
    )

    roundtrip(
      FullTypeCheckCompleteEvent: EnsimeEvent,
      ":full-type-check-complete-event"
    )

    roundtrip(
      IndexerReadyEvent: EnsimeEvent,
      ":indexer-ready-event"
    )

    roundtrip(
      NewScalaNotesEvent(
        isFull = false,
        List(Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
      ): EnsimeEvent,
      """(:new-scala-notes-event (:notes ((:file "foo.scala" :msg "testMsg" :severity :note-warn :beg 50 :end 55 :line 77 :col 5))))"""
    )

    roundtrip(
      ClearAllScalaNotesEvent: EnsimeEvent,
      ":clear-all-scala-notes-event"
    )
  }

  it should "roundtrip DebugEvent as EnsimeEvent" in {
    roundtrip(
      DebugOutputEvent("XXX"): EnsimeEvent,
      """(:debug-output-event (:body "XXX"))"""
    )

    roundtrip(
      DebugStepEvent(DebugThreadId(207), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      s"""(:debug-step-event (:thread-id 207 :thread-name "threadNameStr" :file "$file1" :line 57))"""
    )

    roundtrip(
      DebugBreakEvent(DebugThreadId(209), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      s"""(:debug-break-event (:thread-id 209 :thread-name "threadNameStr" :file "$file1" :line 57))"""
    )

    roundtrip(
      DebugVmStartEvent: EnsimeEvent,
      """:debug-vm-start-event"""
    )

    roundtrip(
      DebugVmDisconnectEvent: EnsimeEvent,
      """:debug-vm-disconnect-event"""
    )

    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", Some(sourcePos1.file), Some(sourcePos1.line)): EnsimeEvent,
      s"""(:debug-exception-event (:exception 33 :thread-id 13 :thread-name "threadNameStr" :file "$file1" :line 57))"""
    )

    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", None, None): EnsimeEvent,
      """(:debug-exception-event (:exception 33 :thread-id 13 :thread-name "threadNameStr"))"""
    )

    roundtrip(
      DebugThreadStartEvent(dtid): EnsimeEvent,
      """(:debug-thread-start-event (:thread-id 13))"""
    )

    roundtrip(
      DebugThreadDeathEvent(dtid): EnsimeEvent,
      """(:debug-thread-death-event (:thread-id 13))"""
    )
  }

  it should "roundtrip DebugLocation" in {
    roundtrip(
      DebugObjectReference(57L): DebugLocation,
      """(:debug-object-reference (:object-id 57))"""
    )

    roundtrip(
      DebugArrayElement(DebugObjectId(58L), 2): DebugLocation,
      """(:debug-array-element (:object-id 58 :index 2))"""
    )

    roundtrip(
      DebugObjectField(DebugObjectId(58L), "fieldName"): DebugLocation,
      """(:debug-object-field (:object-id 58 :field "fieldName"))"""
    )

    roundtrip(
      DebugStackSlot(DebugThreadId(27), 12, 23): DebugLocation,
      """(:debug-stack-slot (:thread-id 27 :frame 12 :offset 23))"""
    )
  }

  it should "roundtrip DebugValue" in {
    roundtrip(
      DebugPrimitiveValue("summaryStr", "typeNameStr"): DebugValue,
      """(:debug-primitive-value (:summary "summaryStr" :type-name "typeNameStr"))"""
    )

    roundtrip(
      DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """(:debug-string-instance (:summary "summaryStr" :fields ((:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")) :type-name "typeNameStr" :object-id 5))"""
    )

    roundtrip(
      DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """(:debug-object-instance (:summary "summaryStr" :fields ((:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")) :type-name "typeNameStr" :object-id 5))"""
    )

    roundtrip(
      DebugNullValue("typeNameStr"): DebugValue,
      """(:debug-null-value (:type-name "typeNameStr"))"""
    )

    roundtrip(
      DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L)): DebugValue,
      """(:debug-array-instance (:length 3 :type-name "typeName" :element-type-name "elementType" :object-id 5))"""
    )

    roundtrip(
      debugClassField: DebugClassField,
      """(:debug-class-field (:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr"))"""
    )

    roundtrip(
      debugStackLocal1: DebugStackLocal,
      """(:debug-stack-local (:index 3 :name "name1" :summary "summary1" :type-name "type1"))"""
    )

    roundtrip(
      debugStackFrame: DebugStackFrame,
      s"""(:debug-stack-frame (:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file "$file1" :line 57) :this-object-id 7))"""
    )

    roundtrip(
      DebugBacktrace(List(debugStackFrame), dtid, "thread1"): DebugBacktrace,
      s"""(:debug-backtrace (:frames ((:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file "$file1" :line 57) :this-object-id 7)) :thread-id 13 :thread-name "thread1"))"""
    )

    roundtrip(
      sourcePos1: SourcePosition,
      s"""(:line-source-position (:file "$file1" :line 57))"""
    )

    roundtrip(
      sourcePos2: SourcePosition,
      s"""(:line-source-position (:file "$file1" :line 59))"""
    )

    roundtrip(
      sourcePos3: SourcePosition,
      ":empty-source-position"
    )

    roundtrip(
      sourcePos4: SourcePosition,
      s"""(:offset-source-position (:file "$file1" :offset 456))"""
    )

    roundtrip(
      breakPoint1: Breakpoint,
      s"""(:breakpoint (:file "$file1" :line 57))"""
    )

    roundtrip(
      BreakpointList(List(breakPoint1), List(breakPoint2)): BreakpointList,
      s"""(:breakpoint-list (:active ((:file "$file1" :line 57)) :pending ((:file "$file1" :line 59))))"""
    )

    roundtrip(
      DebugVmSuccess(): DebugVmStatus,
      """(:debug-vm-success (:status "success"))"""
    )

    roundtrip(
      DebugVmError(303, "xxxx"): DebugVmStatus,
      """(:debug-vm-error (:error-code 303 :details "xxxx" :status "error"))"""
    )
  }

  it should "roundtrip various informational types" in {
    roundtrip(
      note1: Note,
      """(:note (:file "file1" :msg "note1" :severity :note-error :beg 23 :end 33 :line 19 :col 8))"""
    )

    roundtrip(
      completionInfo: CompletionInfo,
      """(:completion-info (:type-info (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")) :name "name" :relevance 90 :to-insert "BAZ"))"""
    )

    roundtrip(
      completionInfo2: CompletionInfo,
      """(:completion-info (:name "name2" :relevance 91))"""
    )

    roundtrip(
      CompletionInfoList("fooBar", List(completionInfo)): CompletionInfoList,
      """(:completion-info-list (:prefix "fooBar" :completions ((:type-info (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")) :name "name" :relevance 90 :to-insert "BAZ"))))"""
    )

    roundtrip(
      SymbolInfo("name", "localName", None, typeInfo): SymbolInfo,
      """(:symbol-info (:name "name" :local-name "localName" :type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1"))))"""
    )

    roundtrip(
      NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method): EntityInfo,
      """(:named-type-member-info (:name "typeX" :type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")) :decl-as :method))"""
    )

    roundtrip(
      entityInfo: EntityInfo,
      """(:arrow-type-info (:name "Arrow1" :full-name "example.Arrow1" :result-type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")) :param-sections ((:params ((:_1 "ABC" :_2 (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1"))))))))"""
    )

    roundtrip(
      entityInfoTypeParams: EntityInfo,
      s"""(:arrow-type-info (:name "Arrow1" :full-name "example.Arrow1" :result-type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")) :param-sections ((:params ((:_1 "ABC" :_2 (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")))))) :type-params ((:basic-type-info (:name "A" :decl-as :nil :full-name "example.Arrow1.A")) (:basic-type-info (:name "B" :decl-as :nil :full-name "example.Arrow1.B")))))"""
    )

    roundtrip(
      typeInfo: EntityInfo,
      """(:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1"))"""
    )

    roundtrip(
      packageInfo: EntityInfo,
      """(:package-info (:name "name" :full-name "fullName"))"""
    )

    roundtrip(
      interfaceInfo: InterfaceInfo,
      """(:interface-info (:type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")) :via-view "DEF"))"""
    )

    roundtrip(
      TypeInspectInfo(typeInfo, List(interfaceInfo)): TypeInspectInfo,
      """(:type-inspect-info (:type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")) :interfaces ((:type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")) :via-view "DEF")) :info-type typeInspect))"""
    )

    roundtrip(
      structureView: StructureView,
      s"""(:structure-view (:view ((:keyword "class" :name "StructureView" :position (:line-source-position (:file "$file1" :line 57))) (:keyword "object" :name "StructureView" :position (:line-source-position (:file "$file1" :line 59)) :members ((:keyword "type" :name "BasicType" :position (:offset-source-position (:file "$file1" :offset 456))))))))"""
    )

    roundtrip(
      astInfo: AstInfo,
      """(:ast-info (:ast "List(Apply(Select(Literal(Constant(1)), TermName(\"$plus\")), List(Literal(Constant(1)))))"))"""
    )
  }

  it should "roundtrip search related responses" in {
    roundtrip(
      SymbolSearchResults(List(methodSearchRes, typeSearchRes)): SymbolSearchResults,
      s"""(:symbol-search-results (:syms ((:method-search-result (:name "abc" :local-name "a" :decl-as :method :pos (:line-source-position (:file "$abd" :line 10)) :owner-name "ownerStr")) (:type-search-result (:name "abc" :local-name "a" :decl-as :trait :pos (:line-source-position (:file "$abd" :line 10)))))))"""
    )

    roundtrip(
      ImportSuggestions(List(List(methodSearchRes, typeSearchRes))): ImportSuggestions,
      s"""(:import-suggestions (:sym-lists (((:method-search-result (:name "abc" :local-name "a" :decl-as :method :pos (:line-source-position (:file "$abd" :line 10)) :owner-name "ownerStr")) (:type-search-result (:name "abc" :local-name "a" :decl-as :trait :pos (:line-source-position (:file "$abd" :line 10))))))))"""
    )

    roundtrip(
      methodSearchRes: SymbolSearchResult,
      s"""(:method-search-result (:name "abc" :local-name "a" :decl-as :method :pos (:line-source-position (:file "$abd" :line 10)) :owner-name "ownerStr"))"""
    )

    roundtrip(
      typeSearchRes: SymbolSearchResult,
      s"""(:type-search-result (:name "abc" :local-name "a" :decl-as :trait :pos (:line-source-position (:file "$abd" :line 10))))"""
    )
  }

  it should "roundtrip ranges and semantic highlighting" in {
    roundtrip(
      ERangePositions(ERangePosition(batchSourceFile, 75, 70, 90) :: Nil),
      """(:e-range-positions (:positions ((:file "/abc" :offset 75 :start 70 :end 90))))"""
    )

    roundtrip(
      FileRange("/abc", 7, 9): FileRange,
      """(:file-range (:file "/abc" :start 7 :end 9))"""
    )

    roundtrip(
      SymbolDesignations(
        symFile, List(
        SymbolDesignation(7, 9, VarFieldSymbol),
        SymbolDesignation(11, 22, ClassSymbol)
      )
      ): SymbolDesignations,
      s"""(:symbol-designations (:file "$symFile" :syms ((:start 7 :end 9 :sym-type :var-field-symbol) (:start 11 :end 22 :sym-type :class-symbol))))"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitConversionInfo(5, 6, symbolInfo))): ImplicitInfos,
      """(:implicit-infos (:infos ((:implicit-conversion-info (:start 5 :end 6 :fun (:name "name" :local-name "localName" :type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1"))))))))"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitParamInfo(5, 6, symbolInfo, List(symbolInfo, symbolInfo), true))): ImplicitInfos,
      s"""(:implicit-infos (:infos ((:implicit-param-info (:start 5 :end 6 :fun (:name "name" :local-name "localName" :type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1"))) :params ((:name "name" :local-name "localName" :type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1"))) (:name "name" :local-name "localName" :type (:basic-type-info (:name "type1" :decl-as :method :full-name "FOO.type1")))) :fun-is-implicit t)))))"""
    )
  }

  it should "roundtrip refactoring messages" in {
    roundtrip(
      RefactorFailure(7, "message"): RefactorFailure,
      """(:refactor-failure (:procedure-id 7 :reason "message" :status failure))"""
    )

    roundtrip(
      refactorDiffEffect: RefactorDiffEffect,
      s"""(:refactor-diff-effect (:procedure-id 9 :refactor-type :add-import :diff "$file2"))"""
    )

  }

  it should "roundtrip legacy raw response types" in {
    roundtrip(
      FalseResponse,
      ":false-response"
    )

    roundtrip(
      TrueResponse,
      ":true-response"
    )

    roundtrip(
      StringResponse("wibble"),
      """(:string-response (:text "wibble"))"""
    )

    roundtrip(
      VoidResponse,
      """:void-response"""
    )

  }

}
