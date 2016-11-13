package com.persist.uw.examples

sealed trait Tree2 {
  def children: Seq[Tree2]

  def size: Int = children.map(_.size).sum + 1

  def depth: Int = if (children.size == 0) 1 else children.map(_.depth).max + 1
}

case class Int2(i: Int) extends Tree2 {
  val children = Seq.empty
}

case class Name2(name: String) extends Tree2 {
  val children = Seq.empty
}

case class Add2(left: Tree2, right: Tree2) extends Tree2{
  val children = Seq(left, right)
}

case class Subtract2(left: Tree2, right: Tree2) extends Tree2 {
  val children = Seq(left, right)
}

case class Multiply2(left: Tree2, right: Tree2) extends Tree2 {
  val children = Seq(left, right)
}

case class Divide2(left: Tree2, right: Tree2) extends Tree2 {
  val children = Seq(left, right)
}

case class If2(test: Tree2, t: Tree2, f: Tree2) extends Tree2 {
  val children = Seq(test, t, f)
}

