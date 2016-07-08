package org.hierarchy

trait Bar

class Foo extends Bar

trait NotBaz extends Runnable

abstract class Qux extends Ordered[Foo] with NotBaz with Bar

trait SomeTrait

class ExtendsTrait extends SomeTrait

class Subclass extends ExtendsTrait

class ExtendsTraitToo extends SomeTrait

