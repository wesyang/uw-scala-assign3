package com.persist.uw.examples

object Tree3 {
  def size(t: Tree3): Int = {
    t match {
      case Int3(_) | Name3(_) => 1
      case Add3(left, right) => size(left) + size(right) + 1
      case Subtract3(left, right) => size(left) + size(right) + 1
      case Multiply3(left, right) => size(left) + size(right) + 1
      case Divide3(left, right) => size(left) + size(right) + 1
      case If3(test, t, f) => size(test) + size(t) + size(f) + 1
    }
  }

  def depth(t: Tree3): Int = {
    t match {
      case Int3(_) | Name3(_) => 1
      case Add3(left, right) => Math.max(depth(left), depth(right)) + 1
      case Subtract3(left, right) => Math.max(depth(left), depth(right)) + 1
      case Multiply3(left, right) => Math.max(depth(left), depth(right)) + 1
      case Divide3(left, right) => Math.max(depth(left), depth(right)) + 1
      case If3(test, t, f) => Math.max(depth(test), Math.max(depth(t), depth(f))) + 1
    }

  }
}

sealed trait Tree3

case class Int3(i: Int) extends Tree3

case class Name3(name: String) extends Tree3

case class Add3(left: Tree3, right: Tree3) extends Tree3

case class Subtract3(left: Tree3, right: Tree3) extends Tree3

case class Multiply3(left: Tree3, right: Tree3) extends Tree3

case class Divide3(left: Tree3, right: Tree3) extends Tree3

case class If3(test: Tree3, t: Tree3, f: Tree3) extends Tree3


