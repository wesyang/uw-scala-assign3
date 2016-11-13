package com.persist.uw.examples


sealed trait Tree1 {
  def size: Int

  def depth: Int
}

case class Int1(i: Int) extends Tree1 {
  def size = 1

  def depth = 1
}

case class Name1(name: String) extends Tree1 {
  def size = 1

  def depth = 1
}

case class Add1(left: Tree1, right: Tree1) extends Tree1 {
  def size = left.size + right.size + 1

  def depth = math.max(left.depth, right.depth) + 1
}

case class Subtract1(left: Tree1, right: Tree1) extends Tree1 {
  def size = left.size + right.size + 1

  def depth = math.max(left.depth, right.depth) + 1
}

case class Multiply1(left: Tree1, right: Tree1) extends Tree1 {
  def size = left.size + right.size + 1

  def depth = math.max(left.depth, right.depth) + 1
}

case class Divide1(left: Tree1, right: Tree1) extends Tree1 {
  def size = left.size + right.size + 1

  def depth = math.max(left.depth, right.depth) + 1
}

case class If1(test: Tree1, t: Tree1, f: Tree1) extends Tree1 {
  def size = test.size + t.size + f.size + 1

  def depth = math.max(test.depth, math.max(t.depth, f.depth)) + 1
}

