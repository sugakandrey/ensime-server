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
    assertFormat(enveloped, s"""(:req $via :callId -1)""".parseSexp)
  }

  def roundtrip(value: EnsimeServerMessage, via: String): Unit = {
    val enveloped = RpcResponseEnvelope(None, value)
    assertFormat(enveloped, s"""(:payload $via)""".parseSexp)
  }

  "SWANK Formats" should "roundtrip startup messages" in {
    roundtrip(
      ConnectionInfoReq: RpcRequest,
      "(:ConnectionInfoReq nil)"
    )
  }

  it should "roundtrip RpcSearchRequests" in {
    roundtrip(
      PublicSymbolSearchReq(List("foo", "bar"), 10): RpcRequest,
      """(:PublicSymbolSearchReq (:keywords ("foo" "bar") :maxResults 10))"""
    )

    roundtrip(
      ImportSuggestionsReq(Left(file1), 1, List("foo", "bar"), 10): RpcRequest,
      s"""(:ImportSuggestionsReq (:file "$file1" :point 1 :names ("foo" "bar") :maxResults 10))"""
    )
  }

  it should "roundtrip RpcAnalyserRequests" in {
    roundtrip(
      RemoveFileReq(file1): RpcRequest,
      s"""(:RemoveFileReq (:file "$file1"))"""
    )

    roundtrip(
      TypecheckFileReq(sourceFileInfo): RpcRequest,
      s"""(:TypecheckFileReq (:fileInfo (:file "$file1" :contents "{/* code here */}" :contentsIn "$file2")))"""
    )

    roundtrip(
      TypecheckFilesReq(List(Left(file1), Left(file2))): RpcRequest,
      s"""(:TypecheckFilesReq (:files ("$file1" "$file2")))"""
    )

    roundtrip(
      TypecheckFilesReq(List(Right(SourceFileInfo(file1)), Right(SourceFileInfo(file2, Some("xxx"), None)))): RpcRequest,
      s"""(:TypecheckFilesReq (:files ((:file "$file1") (:file "$file2" :contents "xxx"))))"""
    )

    roundtrip(
      UnloadAllReq: RpcRequest,
      """(:UnloadAllReq nil)"""
    )

    roundtrip(
      TypecheckAllReq: RpcRequest,
      """(:TypecheckAllReq nil)"""
    )

    roundtrip(
      FormatSourceReq(List(file1, file2)): RpcRequest,
      s"""(:FormatSourceReq (:files ("$file1" "$file2")))"""
    )

    roundtrip(
      FormatOneSourceReq(sourceFileInfo): RpcRequest,
      s"""(:FormatOneSourceReq (:file (:file "$file1" :contents "{/* code here */}" :contentsIn "$file2")))"""
    )

    roundtrip(
      DocUriAtPointReq(Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""(:DocUriAtPointReq (:file "$file1" :point (:from 1 :to 10)))"""
    )

    roundtrip(
      DocUriAtPointReq(Right(SourceFileInfo(file1, None, Some(file2))), OffsetRange(1, 10)): RpcRequest,
      s"""(:DocUriAtPointReq (:file (:file "$file1" :contentsIn "$file2") :point (:from 1 :to 10)))"""
    )

    roundtrip(
      DocUriForSymbolReq("foo.bar", Some("Baz"), None): RpcRequest,
      s"""(:DocUriForSymbolReq (:typeFullName "foo.bar" :memberName "Baz"))"""
    )

    roundtrip(
      CompletionsReq(sourceFileInfo, 10, 100, true, false): RpcRequest,
      s"""(:CompletionsReq (:fileInfo (:file "$file1" :contents "{/* code here */}" :contentsIn "$file2") :point 10 :maxResults 100 :caseSens t))"""
    )

    roundtrip(
      PackageMemberCompletionReq("foo", "bar"): RpcRequest,
      """(:PackageMemberCompletionReq (:path "foo" :prefix "bar"))"""
    )

    roundtrip(
      UsesOfSymbolAtPointReq(Left(file1), 100): RpcRequest,
      s"""(:UsesOfSymbolAtPointReq (:file "$file1" :point 100))"""
    )

    roundtrip(
      TypeByNameReq("foo.bar"): RpcRequest,
      s"""(:TypeByNameReq (:name "foo.bar"))"""
    )

    roundtrip(
      TypeByNameAtPointReq("foo.bar", Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""(:TypeByNameAtPointReq (:name "foo.bar" :file "$file1" :range (:from 1 :to 10)))"""
    )

    roundtrip(
      TypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""(:TypeAtPointReq (:file "$file1" :range (:from 1 :to 100)))"""
    )

    roundtrip(
      InspectTypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""(:InspectTypeAtPointReq (:file "$file1" :range (:from 1 :to 100)))"""
    )

    roundtrip(
      InspectTypeByNameReq("foo.Bar"): RpcRequest,
      s"""(:InspectTypeByNameReq (:name "foo.Bar"))"""
    )

    roundtrip(
      SymbolAtPointReq(Left(file1), 101): RpcRequest,
      s"""(:SymbolAtPointReq (:file "$file1" :point 101))"""
    )

    roundtrip(
      SymbolByNameReq("foo.Bar", Some("baz"), None): RpcRequest,
      s"""(:SymbolByNameReq (:typeFullName "foo.Bar" :memberName "baz"))"""
    )

    roundtrip(
      InspectPackageByPathReq("foo.bar"): RpcRequest,
      s"""(:InspectPackageByPathReq (:path "foo.bar"))"""
    )

    roundtrip(
      RefactorReq(1, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest,
      s"""(:RefactorReq (:procId 1 :params (:RenameRefactorDesc (:newName "bar" :file "$file1" :start 1 :end 100))))"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Left(file1), 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""(:SymbolDesignationsReq (:file "$file1" :start 1 :end 100 :requestedTypes ((:ObjectSymbol nil) (:ValSymbol nil))))"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Right(SourceFileInfo(file1, None, None)), 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""(:SymbolDesignationsReq (:file (:file "$file1") :start 1 :end 100 :requestedTypes ((:ObjectSymbol nil) (:ValSymbol nil))))"""
    )

    roundtrip(
      ExpandSelectionReq(file1, 100, 200): RpcRequest,
      s"""(:ExpandSelectionReq (:file "$file1" :start 100 :end 200))"""
    )

    roundtrip(
      ImplicitInfoReq(Left(file1), OffsetRange(0, 123)),
      s"""(:ImplicitInfoReq (:file "$file1" :range (:from 0 :to 123)))"""
    )

    roundtrip(
      StructureViewReq(sourceFileInfo): RpcRequest,
      s"""(:StructureViewReq (:fileInfo (:file "$file1" :contents "{/* code here */}" :contentsIn "$file2")))"""
    )

    roundtrip(
      AstAtPointReq(sourceFileInfo, OffsetRange(1, 100)): RpcRequest,
      s"""(:AstAtPointReq (:file (:file "$file1" :contents "{/* code here */}" :contentsIn "$file2") :offset (:from 1 :to 100)))"""
    )
  }

  it should "roundtrip RpcDebugRequests" in {
    roundtrip(
      DebugActiveVmReq: RpcRequest,
      """(:DebugActiveVmReq nil)"""
    )

    roundtrip(
      DebugAttachReq("mylovelyhorse", "13"): RpcRequest,
      """(:DebugAttachReq (:hostname "mylovelyhorse" :port "13"))"""
    )

    roundtrip(
      DebugStopReq: RpcRequest,
      """(:DebugStopReq nil)"""
    )

    roundtrip(
      DebugSetBreakReq(file1, 13): RpcRequest,
      s"""(:DebugSetBreakReq (:file "$file1" :line 13))"""
    )

    roundtrip(
      DebugClearBreakReq(file1, 13): RpcRequest,
      s"""(:DebugClearBreakReq (:file "$file1" :line 13))"""
    )

    roundtrip(
      DebugClearAllBreaksReq: RpcRequest,
      s"""(:DebugClearAllBreaksReq nil)"""
    )

    roundtrip(
      DebugListBreakpointsReq: RpcRequest,
      s"""(:DebugListBreakpointsReq nil)"""
    )

    roundtrip(
      DebugRunReq: RpcRequest,
      s"""(:DebugRunReq nil)"""
    )

    roundtrip(
      DebugContinueReq(dtid): RpcRequest,
      s"""(:DebugContinueReq (:threadId (:id 13)))"""
    )

    roundtrip(
      DebugStepReq(dtid): RpcRequest,
      s"""(:DebugStepReq (:threadId (:id 13)))"""
    )

    roundtrip(
      DebugNextReq(dtid): RpcRequest,
      s"""(:DebugNextReq (:threadId (:id 13)))"""
    )

    roundtrip(
      DebugStepOutReq(dtid): RpcRequest,
      s"""(:DebugStepOutReq (:threadId (:id 13)))"""
    )

    roundtrip(
      DebugLocateNameReq(dtid, "foo"): RpcRequest,
      s"""(:DebugLocateNameReq (:threadId (:id 13) :name "foo"))"""
    )

    roundtrip(
      DebugValueReq(debugLocationArray): RpcRequest,
      s"""(:DebugValueReq (:loc (:DebugArrayElement (:objectId (:id 13) :index 14))))"""
    )

    roundtrip(
      DebugToStringReq(dtid, debugLocationArray): RpcRequest,
      s"""(:DebugToStringReq (:threadId (:id 13) :loc (:DebugArrayElement (:objectId (:id 13) :index 14))))"""
    )

    roundtrip(
      DebugSetValueReq(debugLocationArray, "bar"): RpcRequest,
      s"""(:DebugSetValueReq (:loc (:DebugArrayElement (:objectId (:id 13) :index 14)) :newValue "bar"))"""
    )

    roundtrip(
      DebugBacktraceReq(dtid, 100, 200): RpcRequest,
      s"""(:DebugBacktraceReq (:threadId (:id 13) :index 100 :count 200))"""
    )

  }

  it should "roundtrip EnsimeGeneralEvent as EnsimeEvent" in {
    roundtrip(
      SendBackgroundMessageEvent("ABCDEF", 1): EnsimeEvent,
      """(:SendBackgroundMessageEvent (:detail "ABCDEF" :code 1))"""
    )

    roundtrip(
      AnalyzerReadyEvent: EnsimeEvent,
      "(:AnalyzerReadyEvent nil)"
    )

    roundtrip(
      FullTypeCheckCompleteEvent: EnsimeEvent,
      "(:FullTypeCheckCompleteEvent nil)"
    )

    roundtrip(
      IndexerReadyEvent: EnsimeEvent,
      "(:IndexerReadyEvent nil)"
    )

    roundtrip(
      NewScalaNotesEvent(
        isFull = false,
        List(Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
      ): EnsimeEvent,
      """(:NewScalaNotesEvent (:notes ((:file "foo.scala" :msg "testMsg" :severity (:NoteWarn nil) :beg 50 :end 55 :line 77 :col 5))))"""
    )

    roundtrip(
      ClearAllScalaNotesEvent: EnsimeEvent,
      "(:ClearAllScalaNotesEvent nil)"
    )
  }

  it should "roundtrip DebugEvent as EnsimeEvent" in {
    roundtrip(
      DebugOutputEvent("XXX"): EnsimeEvent,
      """(:DebugOutputEvent (:body "XXX"))"""
    )

    roundtrip(
      DebugStepEvent(DebugThreadId(207), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      s"""(:DebugStepEvent (:threadId (:id 207) :threadName "threadNameStr" :file "$file1" :line 57))"""
    )

    roundtrip(
      DebugBreakEvent(DebugThreadId(209), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      s"""(:DebugBreakEvent (:threadId (:id 209) :threadName "threadNameStr" :file "$file1" :line 57))"""
    )

    roundtrip(
      DebugVMStartEvent: EnsimeEvent,
      """(:DebugVMStartEvent nil)"""
    )

    roundtrip(
      DebugVMDisconnectEvent: EnsimeEvent,
      """(:DebugVMDisconnectEvent nil)"""
    )

    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", Some(sourcePos1.file), Some(sourcePos1.line)): EnsimeEvent,
      s"""(:DebugExceptionEvent (:exception 33 :threadId (:id 13) :threadName "threadNameStr" :file "$file1" :line 57))"""
    )

    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", None, None): EnsimeEvent,
      """(:DebugExceptionEvent (:exception 33 :threadId (:id 13) :threadName "threadNameStr"))"""
    )

    roundtrip(
      DebugThreadStartEvent(dtid): EnsimeEvent,
      """(:DebugThreadStartEvent (:threadId (:id 13)))"""
    )

    roundtrip(
      DebugThreadDeathEvent(dtid): EnsimeEvent,
      """(:DebugThreadDeathEvent (:threadId (:id 13)))"""
    )
  }

  it should "roundtrip DebugLocation" in {
    roundtrip(
      DebugObjectReference(57L): DebugLocation,
      """(:DebugObjectReference (:objectId (:id 57)))"""
    )

    roundtrip(
      DebugArrayElement(DebugObjectId(58L), 2): DebugLocation,
      """(:DebugArrayElement (:objectId (:id 58) :index 2))"""
    )

    roundtrip(
      DebugObjectField(DebugObjectId(58L), "fieldName"): DebugLocation,
      """(:DebugObjectField (:objectId (:id 58) :field "fieldName"))"""
    )

    roundtrip(
      DebugStackSlot(DebugThreadId(27), 12, 23): DebugLocation,
      """(:DebugStackSlot (:threadId (:id 27) :frame 12 :offset 23))"""
    )
  }

  it should "roundtrip DebugValue" in {
    roundtrip(
      DebugPrimitiveValue("summaryStr", "typeNameStr"): DebugValue,
      """(:DebugPrimitiveValue (:summary "summaryStr" :typeName "typeNameStr"))"""
    )

    roundtrip(
      DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """(:DebugStringInstance (:summary "summaryStr" :fields ((:index 19 :name "nameStr" :typeName "typeNameStr" :summary "summaryStr")) :typeName "typeNameStr" :objectId (:id 5)))"""
    )

    roundtrip(
      DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """(:DebugObjectInstance (:summary "summaryStr" :fields ((:index 19 :name "nameStr" :typeName "typeNameStr" :summary "summaryStr")) :typeName "typeNameStr" :objectId (:id 5)))"""
    )

    roundtrip(
      DebugNullValue("typeNameStr"): DebugValue,
      """(:DebugNullValue (:typeName "typeNameStr"))"""
    )

    roundtrip(
      DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L)): DebugValue,
      """(:DebugArrayInstance (:length 3 :typeName "typeName" :elementTypeName "elementType" :objectId (:id 5)))"""
    )

    roundtrip(
      debugClassField: DebugClassField,
      """(:DebugClassField (:index 19 :name "nameStr" :typeName "typeNameStr" :summary "summaryStr"))"""
    )

    roundtrip(
      debugStackLocal1: DebugStackLocal,
      """(:DebugStackLocal (:index 3 :name "name1" :summary "summary1" :typeName "type1"))"""
    )

    roundtrip(
      debugStackFrame: DebugStackFrame,
      s"""(:DebugStackFrame (:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :typeName "type1") (:index 4 :name "name2" :summary "summary2" :typeName "type2")) :numArgs 4 :className "class1" :methodName "method1" :pcLocation (:file "$file1" :line 57) :thisObjectId (:id 7)))"""
    )

    roundtrip(
      DebugBacktrace(List(debugStackFrame), dtid, "thread1"): DebugBacktrace,
      s"""(:DebugBacktrace (:frames ((:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :typeName "type1") (:index 4 :name "name2" :summary "summary2" :typeName "type2")) :numArgs 4 :className "class1" :methodName "method1" :pcLocation (:file "$file1" :line 57) :thisObjectId (:id 7))) :threadId (:id 13) :threadName "thread1"))"""
    )

    roundtrip(
      sourcePos1: SourcePosition,
      s"""(:LineSourcePosition (:file "$file1" :line 57))"""
    )

    roundtrip(
      sourcePos2: SourcePosition,
      s"""(:LineSourcePosition (:file "$file1" :line 59))"""
    )

    roundtrip(
      sourcePos3: SourcePosition,
      "(:EmptySourcePosition nil)"
    )

    roundtrip(
      sourcePos4: SourcePosition,
      s"""(:OffsetSourcePosition (:file "$file1" :offset 456))"""
    )

    roundtrip(
      breakPoint1: Breakpoint,
      s"""(:Breakpoint (:file "$file1" :line 57))"""
    )

    roundtrip(
      BreakpointList(List(breakPoint1), List(breakPoint2)): BreakpointList,
      s"""(:BreakpointList (:active ((:file "$file1" :line 57)) :pending ((:file "$file1" :line 59))))"""
    )

    roundtrip(
      DebugVmSuccess(): DebugVmStatus,
      """(:DebugVmSuccess (:status "success"))"""
    )

    roundtrip(
      DebugVmError(303, "xxxx"): DebugVmStatus,
      """(:DebugVmError (:errorCode 303 :details "xxxx" :status "error"))"""
    )
  }

  it should "roundtrip various informational types" in {
    roundtrip(
      note1: Note,
      """(:Note (:file "file1" :msg "note1" :severity (:NoteError nil) :beg 23 :end 33 :line 19 :col 8))"""
    )

    roundtrip(
      completionInfo: CompletionInfo,
      """(:CompletionInfo (:typeInfo (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")) :name "name" :relevance 90 :toInsert "BAZ"))"""
    )

    roundtrip(
      completionInfo2: CompletionInfo,
      """(:CompletionInfo (:name "name2" :relevance 91))"""
    )

    roundtrip(
      CompletionInfoList("fooBar", List(completionInfo)): CompletionInfoList,
      """(:CompletionInfoList (:prefix "fooBar" :completions ((:typeInfo (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")) :name "name" :relevance 90 :toInsert "BAZ"))))"""
    )

    roundtrip(
      SymbolInfo("name", "localName", None, typeInfo): SymbolInfo,
      """(:SymbolInfo (:name "name" :localName "localName" :type (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1"))))"""
    )

    roundtrip(
      NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method): EntityInfo,
      """(:NamedTypeMemberInfo (:name "typeX" :type (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")) :declAs (:Method nil)))"""
    )

    roundtrip(
      entityInfo: EntityInfo,
      """(:ArrowTypeInfo (:name "Arrow1" :fullName "example.Arrow1" :resultType (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")) :paramSections ((:params ((:_1 "ABC" :_2 (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1"))))))))"""
    )

    roundtrip(
      entityInfoTypeParams: EntityInfo,
      s"""(:ArrowTypeInfo (:name "Arrow1" :fullName "example.Arrow1" :resultType (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")) :paramSections ((:params ((:_1 "ABC" :_2 (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")))))) :typeParams ((:BasicTypeInfo (:name "A" :declAs (:Nil nil) :fullName "example.Arrow1.A")) (:BasicTypeInfo (:name "B" :declAs (:Nil nil) :fullName "example.Arrow1.B")))))"""
    )

    roundtrip(
      typeInfo: EntityInfo,
      """(:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1"))"""
    )

    roundtrip(
      packageInfo: EntityInfo,
      """(:PackageInfo (:name "name" :fullName "fullName"))"""
    )

    roundtrip(
      interfaceInfo: InterfaceInfo,
      """(:InterfaceInfo (:type (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")) :viaView "DEF"))"""
    )

    roundtrip(
      TypeInspectInfo(typeInfo, List(interfaceInfo)): TypeInspectInfo,
      """(:TypeInspectInfo (:type (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")) :interfaces ((:type (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")) :viaView "DEF")) :infoType typeInspect))"""
    )

    roundtrip(
      structureView: StructureView,
      s"""(:StructureView (:view ((:keyword "class" :name "StructureView" :position (:LineSourcePosition (:file "$file1" :line 57))) (:keyword "object" :name "StructureView" :position (:LineSourcePosition (:file "$file1" :line 59)) :members ((:keyword "type" :name "BasicType" :position (:OffsetSourcePosition (:file "$file1" :offset 456))))))))"""
    )

    roundtrip(
      astInfo: AstInfo,
      """(:AstInfo (:ast "List(Apply(Select(Literal(Constant(1)), TermName(\"$plus\")), List(Literal(Constant(1)))))"))"""
    )
  }

  it should "roundtrip search related responses" in {
    roundtrip(
      SymbolSearchResults(List(methodSearchRes, typeSearchRes)): SymbolSearchResults,
      s"""(:SymbolSearchResults (:syms ((:MethodSearchResult (:name "abc" :localName "a" :declAs (:Method nil) :pos (:LineSourcePosition (:file "$abd" :line 10)) :ownerName "ownerStr")) (:TypeSearchResult (:name "abc" :localName "a" :declAs (:Trait nil) :pos (:LineSourcePosition (:file "$abd" :line 10)))))))"""
    )

    roundtrip(
      ImportSuggestions(List(List(methodSearchRes, typeSearchRes))): ImportSuggestions,
      s"""(:ImportSuggestions (:symLists (((:MethodSearchResult (:name "abc" :localName "a" :declAs (:Method nil) :pos (:LineSourcePosition (:file "$abd" :line 10)) :ownerName "ownerStr")) (:TypeSearchResult (:name "abc" :localName "a" :declAs (:Trait nil) :pos (:LineSourcePosition (:file "$abd" :line 10))))))))"""
    )

    roundtrip(
      methodSearchRes: SymbolSearchResult,
      s"""(:MethodSearchResult (:name "abc" :localName "a" :declAs (:Method nil) :pos (:LineSourcePosition (:file "$abd" :line 10)) :ownerName "ownerStr"))"""
    )

    roundtrip(
      typeSearchRes: SymbolSearchResult,
      s"""(:TypeSearchResult (:name "abc" :localName "a" :declAs (:Trait nil) :pos (:LineSourcePosition (:file "$abd" :line 10))))"""
    )
  }

  it should "roundtrip ranges and semantic highlighting" in {
    roundtrip(
      ERangePositions(ERangePosition(batchSourceFile, 75, 70, 90) :: Nil),
      """(:ERangePositions (:positions ((:file "/abc" :offset 75 :start 70 :end 90))))"""
    )

    roundtrip(
      FileRange("/abc", 7, 9): FileRange,
      """(:FileRange (:file "/abc" :start 7 :end 9))"""
    )

    roundtrip(
      SymbolDesignations(
        symFile, List(
        SymbolDesignation(7, 9, VarFieldSymbol),
        SymbolDesignation(11, 22, ClassSymbol)
      )
      ): SymbolDesignations,
      s"""(:SymbolDesignations (:file "$symFile" :syms ((:start 7 :end 9 :symType (:VarFieldSymbol nil)) (:start 11 :end 22 :symType (:ClassSymbol nil)))))"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitConversionInfo(5, 6, symbolInfo))): ImplicitInfos,
      """(:ImplicitInfos (:infos ((:ImplicitConversionInfo (:start 5 :end 6 :fun (:name "name" :localName "localName" :type (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1"))))))))"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitParamInfo(5, 6, symbolInfo, List(symbolInfo, symbolInfo), true))): ImplicitInfos,
      s"""(:ImplicitInfos (:infos ((:ImplicitParamInfo (:start 5 :end 6 :fun (:name "name" :localName "localName" :type (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1"))) :params ((:name "name" :localName "localName" :type (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1"))) (:name "name" :localName "localName" :type (:BasicTypeInfo (:name "type1" :declAs (:Method nil) :fullName "FOO.type1")))) :funIsImplicit t)))))"""
    )
  }

  it should "roundtrip refactoring messages" in {
    roundtrip(
      RefactorFailure(7, "message"): RefactorFailure,
      """(:RefactorFailure (:procedureId 7 :reason "message" :status failure))"""
    )

    roundtrip(
      refactorDiffEffect: RefactorDiffEffect,
      s"""(:RefactorDiffEffect (:procedureId 9 :refactorType (:AddImport nil) :diff "$file2"))"""
    )

  }

  it should "roundtrip legacy raw response types" in {
    roundtrip(
      FalseResponse,
      "(:FalseResponse nil)"
    )

    roundtrip(
      TrueResponse,
      "(:TrueResponse nil)"
    )

    roundtrip(
      StringResponse("wibble"),
      """(:StringResponse (:text "wibble"))"""
    )

    roundtrip(
      VoidResponse,
      """(:VoidResponse nil)"""
    )

  }

}
