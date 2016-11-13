package com.persist.uw.examples

object Tree4 {

  sealed trait Tree4

  case class Int4(i: Int) extends Tree4

  case class Name4(name: String) extends Tree4

  case class Add4(left: Tree4, right: Tree4) extends Tree4

  case class Subtract4(left: Tree4, right: Tree4) extends Tree4

  case class Multiply4(left: Tree4, right: Tree4) extends Tree4

  case class Divide4(left: Tree4, right: Tree4) extends Tree4

  case class If4(test: Tree4, t: Tree4, f: Tree4) extends Tree4

}

object Tree4Extend {

  import Tree4._

  trait Extend {
    def size: Int

    def depth: Int
  }

  implicit class ExtendTree(tree: Tree4) extends Extend {
    val size:Int = tree match {
      case i: Int4 => i.size
      case n: Name4 => n.size
      case a: Add4 => a.size
      case s: Subtract4 => s.size
      case m: Multiply4 => m.size
      case d: Divide4 => d.size
      case i: If4 => i.size
    }
    val depth:Int = tree match {
      case i: Int4 => i.depth
      case n: Name4 => n.depth
      case a: Add4 => a.depth
      case s: Subtract4 => s.depth
      case m: Multiply4 => m.depth
      case d: Divide4 => d.depth
      case i: If4 => i.depth
    }
  }

  implicit class ExtendName4(name: Name4) extends Extend {
    val size = 1
    val depth = 1
  }

  implicit class ExtendInt4(int: Int4) extends Extend {
    val size = 1
    val depth = 1
  }

  implicit class ExtendAdd4(add: Add4) extends Extend {
    val size = add.left.size + add.right.size + 1
    val depth = Math.max(add.left.depth, add.right.depth) + 1
  }

  implicit class ExtendSubtract4(add: Subtract4) extends Extend {
    val size = add.left.size + add.right.size + 1
    val depth = Math.max(add.left.depth, add.right.depth) + 1
  }

  implicit class ExtendMultiply4(add: Multiply4) extends Extend {
    val size = add.left.size + add.right.size + 1
    val depth = Math.max(add.left.depth, add.right.depth) + 1
  }

  implicit class ExtendDivide4(add: Divide4) extends Extend {
    val size = add.left.size + add.right.size + 1
    val depth = Math.max(add.left.depth, add.right.depth) + 1
  }

  implicit class ExtendIf4(if3: If4) extends Extend {
    val size = if3.test.size + if3.t.size + if3.f.size + 1
    val depth = Math.max(if3.test.depth, Math.max(if3.t.depth, if3.f.depth)) + 1
  }
}
