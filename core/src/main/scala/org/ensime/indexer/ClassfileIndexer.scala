// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2.FileObject
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._

import scala.collection.immutable.Queue

trait ClassfileIndexer {
  this: SLF4JLogging =>

  /**
   * @param file to index
   * @return the parsed version of the classfile and FQNs referenced within
   */
  def indexClassfile(file: FileObject): RawClassfile = {
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

    raw.clazz
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

      val interfaceNames: List[ClassName] = interfaces.map(ClassName.fromInternal)(collection.breakOut)
      val superClass = Option(superName).map(ClassName.fromInternal)

      internalRefs = internalRefs ++ interfaceNames
      internalRefs = internalRefs ++ superClass.toList

      clazz = RawClassfile(
        ClassName.fromInternal(name),
        signatureClass,
        superClass,
        interfaceNames,
        Access(access),
        (ACC_DEPRECATED & access) > 0,
        Nil, Queue.empty, RawSource(None, None),
        isScala = false,
        internalRefs
      )
    }

    override def visitSource(filename: String, debug: String): Unit = {
      val isScala = filename != null && filename.endsWith(".scala")
      clazz = clazz.copy(source = RawSource(Option(filename), None), isScala = isScala)
    }

    override def visitField(access: Int, name: String, desc: String, signature: String, value: AnyRef): FieldVisitor = {
      super.visitField(access, name, desc, signature, value)
      new FieldVisitor(ASM5) with ReferenceInFieldHunter {
        override def visitEnd(): Unit = {
          internalRefs = internalRefs + ClassName.fromDescriptor(desc)
          val field = RawField(
            FieldName(clazz.name, name),
            DescriptorParser.parseType(desc),
            Option(signature), Access(access),
            internalRefs
          )
          clazz = clazz.copy(fields = field :: clazz.fields)
        }
      }
    }

    override def visitMethod(access: Int, region: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      super.visitMethod(access, region, desc, signature, exceptions)
      new MethodVisitor(ASM5) with ReferenceInMethodHunter {
        var firstLine: Option[Int] = None

        override def visitLineNumber(line: Int, start: Label): Unit = {
          val isEarliestLineSeen = firstLine.forall(_ < line)
          if (isEarliestLineSeen)
            firstLine = Some(line)
        }

        override def visitEnd(): Unit = {
          if (region == "<init>" || region == "<clinit>") {
            (clazz.source.line, firstLine) match {
              case (_, None) =>
              case (Some(existing), Some(latest)) if existing <= latest =>
              case _ =>
                clazz = clazz.copy(source = clazz.source.copy(line = firstLine))
            }
          }
          if (exceptions != null) {
            addRefs(exceptions.map(ClassName.fromInternal))
          }
          addRefs(classesInDescriptor(desc))
          val descriptor = DescriptorParser.parse(desc)
          val method = RawMethod(
            MethodName(clazz.name, region, descriptor),
            Access(access),
            Option(signature),
            firstLine,
            internalRefs
          )
          clazz = clazz.copy(methods = clazz.methods enqueue method)
        }
      }
    }
  }

  // factors out much of the verbose code that looks for references to members
  private trait ReferenceInClassHunter {
    this: ClassVisitor =>

    var clazz: RawClassfile

    var internalRefs = Set.empty[FullyQualifiedName]

    private val annVisitor: AnnotationVisitor = new AnnotationVisitor(ASM5) {
      override def visitAnnotation(name: String, desc: String) = handleAnn(desc)
      override def visitEnum(
        name: String, desc: String, value: String
      ): Unit = handleAnn(desc)
    }

    private def handleAnn(desc: String): AnnotationVisitor = {
      clazz = clazz.copy(internalRefs = clazz.internalRefs + ClassName.fromDescriptor(desc))
      annVisitor
    }
    override def visitAnnotation(desc: String, visible: Boolean) = handleAnn(desc)
    override def visitTypeAnnotation(
      typeRef: Int, typePath: TypePath, desc: String, visible: Boolean
    ) = handleAnn(desc)
  }

  private trait ReferenceInFieldHunter {
    this: FieldVisitor =>

    protected var internalRefs = Set.empty[FullyQualifiedName]

    private val annVisitor = new AnnotationVisitor(ASM5) {
      override def visitAnnotation(name: String, desc: String): AnnotationVisitor = handleAnn(desc)
    }
    private def handleAnn(desc: String): AnnotationVisitor = {
      internalRefs = internalRefs + ClassName.fromDescriptor(desc)
      annVisitor
    }
    override def visitTypeAnnotation(typeRef: Int, typePath: TypePath, desc: String, visible: Boolean): AnnotationVisitor = handleAnn(desc)
    override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = handleAnn(desc)
  }

  private trait ReferenceInMethodHunter {
    this: MethodVisitor =>

    protected var internalRefs = Set.empty[FullyQualifiedName]

    // doesn't disambiguate FQNs of methods, so storing as FieldName references
    private def memberOrInit(owner: String, name: String): FullyQualifiedName =
      name match {
        case "<init>" | "<clinit>" => ClassName.fromInternal(owner)
        case member => FieldName(ClassName.fromInternal(owner), member)
      }

    protected def addRefs(refs: Seq[FullyQualifiedName]): Unit = internalRefs = internalRefs ++ refs

    protected def addRef(ref: FullyQualifiedName): Unit = addRefs(Seq(ref))

    override def visitLocalVariable(
      name: String, desc: String, signature: String,
      start: Label, end: Label, index: Int
    ): Unit = {
      addRef(ClassName.fromDescriptor(desc))
    }

    override def visitMultiANewArrayInsn(desc: String, dims: Int): Unit =
      addRef(ClassName.fromDescriptor(desc))

    override def visitTypeInsn(opcode: Int, desc: String): Unit =
      addRef(ClassName.fromInternal(desc))

    override def visitFieldInsn(
      opcode: Int, owner: String, name: String, desc: String
    ): Unit = {
      addRef(memberOrInit(owner, name))
    }

    override def visitTryCatchBlock(start: Label, end: Label, handler: Label, `type`: String): Unit = {
      Option(`type`).foreach(desc => addRef(ClassName.fromInternal(desc)))
    }

    override def visitMethodInsn(
      opcode: Int, owner: String, name: String, desc: String, itf: Boolean
    ): Unit = {
      addRef(MethodName(ClassName.fromInternal(owner), name, DescriptorParser.parse(desc)))
      addRefs(classesInDescriptor(desc))
    }

    override def visitInvokeDynamicInsn(name: String, desc: String, bsm: Handle, bsmArgs: AnyRef*): Unit = {
      addRef(memberOrInit(bsm.getOwner, bsm.getName))
      addRefs(classesInDescriptor(bsm.getDesc))
    }

    private val annVisitor: AnnotationVisitor = new AnnotationVisitor(ASM5) {
      override def visitAnnotation(name: String, desc: String) = handleAnn(desc)
      override def visitEnum(name: String, desc: String, value: String): Unit = handleAnn(desc)
    }
    private def handleAnn(desc: String): AnnotationVisitor = {
      addRef(ClassName.fromDescriptor(desc))
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
