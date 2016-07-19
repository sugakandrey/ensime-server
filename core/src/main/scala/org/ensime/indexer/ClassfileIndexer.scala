// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import scala.collection.immutable.Queue

import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2.FileObject
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._

trait ClassfileIndexer {
  this: SLF4JLogging =>

  /**
   * @param file to index
   * @return the parsed version of the classfile and FQNs referenced within
   */
  def indexClassfile(file: FileObject): (RawClassfile, Set[FullyQualifiedName]) = {
    val name = file.getName
    require(file.exists(), s"$name does not exist")
    require(name.getBaseName.endsWith(".class"), s"$name is not a class file")

    val in = file.getContent.getInputStream
    val raw = try {
      val reader = new ClassReader(in)
      val receiver = new AsmCallback
      reader.accept(receiver, ClassReader.SKIP_FRAMES)
      receiver
    } finally in.close()

    (raw.clazz, raw.refs)
  }

  // extracts all the classnames from a descriptor
  private def classesInDescriptor(desc: String): List[ClassName] =
    DescriptorParser.parse(desc) match {
      case Descriptor(params, ret) => (ret :: params).map {
        case c: ClassName => c
        case a: ArrayDescriptor => a.reifier
      }
    }

  private class AsmCallback extends ClassVisitor(ASM5) with ReferenceInClassHunter {
    // updated every time we get more info
    @volatile var clazz: RawClassfile = _

    override def visit(
      version: Int, access: Int, name: String, signature: String,
      superName: String, interfaces: Array[String]
    ): Unit = {

      val signatureClass = if (signature != null && signature.nonEmpty) {
        Some(SignatureParser.parseGeneric(signature))
      } else {
        None
      }

      clazz = RawClassfile(
        ClassName.fromInternal(name),
        signatureClass,
        Option(superName).map(ClassName.fromInternal),
        interfaces.toList.map(ClassName.fromInternal),
        Access(access),
        (ACC_DEPRECATED & access) > 0,
        Nil, Nil, RawSource(None, None)
      )
    }

    override def visitSource(filename: String, debug: String): Unit = {
      clazz = clazz.copy(source = RawSource(Option(filename), None))
    }

    override def visitField(access: Int, name: String, desc: String, signature: String, value: AnyRef): FieldVisitor = {
      val field = RawField(
        FieldName(clazz.name, name),
        DescriptorParser.parseType(desc),
        Option(signature), Access(access)
      )
      clazz = clazz.copy(fields = field :: clazz.fields)
      super.visitField(access, name, desc, signature, value)
    }

    override def visitMethod(access: Int, region: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      super.visitMethod(access, region, desc, signature, exceptions)
      new MethodVisitor(ASM5) with ReferenceInMethodHunter {
        var firstLine: Option[Int] = None

        override def visitLineNumber(line: Int, start: Label): Unit = {
          val isEarliestLineSeen = firstLine.map(_ < line).getOrElse(true)
          if (isEarliestLineSeen)
            firstLine = Some(line)
        }

        override def visitEnd(): Unit = {
          addRefs(internalRefs)
          region match {
            case "<init>" | "<clinit>" =>
              (clazz.source.line, firstLine) match {
                case (_, None) =>
                case (Some(existing), Some(latest)) if existing <= latest =>
                case _ =>
                  clazz = clazz.copy(source = clazz.source.copy(line = firstLine))
              }
            case name if name.contains("default") =>
            case name =>
              val descriptor = DescriptorParser.parse(desc)
              val method = RawMethod(MethodName(clazz.name, name, descriptor), Access(access), Option(signature), firstLine, clazz.methods.size)
              clazz = clazz.copy(methods = method :: clazz.methods)
          }
        }
      }
    }
  }

  // factors out much of the verbose code that looks for references to members
  private trait ReferenceInClassHunter {
    this: ClassVisitor =>

    def clazz: RawClassfile

    // NOTE: only mutate via addRefs
    var refs = Set.empty[FullyQualifiedName]

    protected def addRefs(seen: Seq[FullyQualifiedName]): Unit = {
      refs ++= seen.filterNot(_.contains(clazz.name))
    }
    protected def addRef(seen: FullyQualifiedName): Unit = addRefs(seen :: Nil)

    private val fieldVisitor = new FieldVisitor(ASM5) {
      override def visitAnnotation(desc: String, visible: Boolean) = handleAnn(desc)
      override def visitTypeAnnotation(
        typeRef: Int, typePath: TypePath, desc: String, visible: Boolean
      ) = handleAnn(desc)
    }

    override def visitField(access: Int, name: String, desc: String, signature: String, value: AnyRef): FieldVisitor = {
      addRef(ClassName.fromDescriptor(desc))
      fieldVisitor
    }

    override def visitMethod(access: Int, region: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      addRefs(classesInDescriptor(desc))
      if (exceptions != null)
        addRefs(exceptions.map(ClassName.fromInternal))
      null
    }

    override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int): Unit = {
      addRef(ClassName.fromInternal(name))
    }

    override def visitOuterClass(owner: String, name: String, desc: String): Unit = {
      addRef(ClassName.fromInternal(owner))
    }

    private val annVisitor: AnnotationVisitor = new AnnotationVisitor(ASM5) {
      override def visitAnnotation(name: String, desc: String) = handleAnn(desc)
      override def visitEnum(
        name: String, desc: String, value: String
      ): Unit = handleAnn(desc)
    }
    private def handleAnn(desc: String): AnnotationVisitor = {
      addRef(ClassName.fromDescriptor(desc))
      annVisitor
    }
    override def visitAnnotation(desc: String, visible: Boolean) = handleAnn(desc)
    override def visitTypeAnnotation(
      typeRef: Int, typePath: TypePath, desc: String, visible: Boolean
    ) = handleAnn(desc)
  }

  private trait ReferenceInMethodHunter {
    this: MethodVisitor =>

    // NOTE: :+ and :+= are really slow (scala 2.10), prefer "enqueue"
    protected var internalRefs = Queue.empty[FullyQualifiedName]

    // doesn't disambiguate FQNs of methods, so storing as FieldName references
    private def memberOrInit(owner: String, name: String): FullyQualifiedName =
      name match {
        case "<init>" | "<clinit>" => ClassName.fromInternal(owner)
        case member => FieldName(ClassName.fromInternal(owner), member)
      }

    override def visitLocalVariable(
      name: String, desc: String, signature: String,
      start: Label, end: Label, index: Int
    ): Unit = {
      internalRefs = internalRefs enqueue ClassName.fromDescriptor(desc)
    }

    override def visitMultiANewArrayInsn(desc: String, dims: Int): Unit = {
      internalRefs = internalRefs enqueue ClassName.fromDescriptor(desc)
    }

    override def visitTypeInsn(opcode: Int, desc: String): Unit = {
      internalRefs = internalRefs enqueue ClassName.fromInternal(desc)
    }

    override def visitFieldInsn(
      opcode: Int, owner: String, name: String, desc: String
    ): Unit = {
      internalRefs = internalRefs enqueue memberOrInit(owner, name)
      internalRefs = internalRefs enqueue ClassName.fromDescriptor(desc)
    }

    override def visitMethodInsn(
      opcode: Int, owner: String, name: String, desc: String, itf: Boolean
    ): Unit = {
      internalRefs :+= memberOrInit(owner, name)
      internalRefs = internalRefs.enqueue(classesInDescriptor(desc))
    }

    override def visitInvokeDynamicInsn(name: String, desc: String, bsm: Handle, bsmArgs: AnyRef*): Unit = {
      internalRefs :+= memberOrInit(bsm.getOwner, bsm.getName)
      internalRefs = internalRefs.enqueue(classesInDescriptor(bsm.getDesc))
    }

    private val annVisitor: AnnotationVisitor = new AnnotationVisitor(ASM5) {
      override def visitAnnotation(name: String, desc: String) = handleAnn(desc)
      override def visitEnum(name: String, desc: String, value: String): Unit = handleAnn(desc)
    }
    private def handleAnn(desc: String): AnnotationVisitor = {
      internalRefs = internalRefs enqueue ClassName.fromDescriptor(desc)
      annVisitor
    }
    override def visitAnnotation(desc: String, visible: Boolean) = handleAnn(desc)
    override def visitAnnotationDefault() = annVisitor
    override def visitInsnAnnotation(
      typeRef: Int, typePath: TypePath, desc: String, visible: Boolean
    ) = handleAnn(desc)
    override def visitLocalVariableAnnotation(
      typeRef: Int, typePath: TypePath, start: Array[Label], end: Array[Label],
      index: Array[Int], desc: String, visible: Boolean
    ) = handleAnn(desc)
    override def visitParameterAnnotation(
      parameter: Int, desc: String, visible: Boolean
    ) = handleAnn(desc)
    override def visitTryCatchAnnotation(
      typeRef: Int, typePath: TypePath, desc: String, visible: Boolean
    ) = handleAnn(desc)
    override def visitTypeAnnotation(
      typeRef: Int, typePath: TypePath, desc: String, visible: Boolean
    ) = handleAnn(desc)
  }
}
