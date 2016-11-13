package com.persist.uw.examples.simplifyRuleSetsCore

import com.persist.uw.examples._

/**
  * Created by wesyang on 11/4/2016.
  */
object SimplifyMultiplyRules extends SimplifyRuleSets{

  override def createTree(left: Tree3, right: Tree3): Tree3 =  {Multiply3(left, right)}

  override def splitTree(tree: Tree3): (Tree3, Tree3) = {
    tree match {
      case Multiply3(left, right) => (left, right)
      case _ => throw new IllegalArgumentException("tree is not Multiply3 node")
    }
  }

  /** ************************************************************************************************
    * return set of functions.
    * each function contains the rules to simplify Multiply3 expression
    */
  override def buildRules(): List[(Tree3, (Tree3, Int)=> Tree3, Int) => Tree3] = {
    List[(Tree3, (Tree3, Int)=> Tree3, Int) => Tree3](
      simplifyBasic,
      simplifyBothSide,
      simplifyDistributive,
      simplifyAssociativeAndCommutative,
      simplifDivisionCancel
    )
  }

  def simplifyBasic (tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3, id:Int): Tree3 = {
    tree match {
      case Multiply3(Int3(0), right) => Int3(0)
      case Multiply3(left, Int3(0)) => Int3(0)
      case Multiply3(Int3(1), right) => simplifyMore(right, id + 1)
      case Multiply3(left, Int3(1)) => simplifyMore(left, id + 1)
      case Multiply3(Int3(x), Int3(y)) => Int3(x * y)

      case Multiply3(Name3(_), Int3(_)) => tree
      case Multiply3(Int3(x), right) => simplifyMore(Multiply3(right, Int3(x)), id + 1) // normalize

      case _ => tree
    }
  }

  def simplifyDistributive(tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3,id: Int): Tree3 = {
    tree match {
      case Multiply3(Multiply3(left, Int3(x)), Int3(y))
        => simplifyMore( Multiply3(left, Int3 (x * y)), id +1)
      case Multiply3(Multiply3(left1, Int3(x1)), Multiply3(left2, Int3(x2)))
        => simplifyMore( Multiply3 (Multiply3(left1, left2), Int3(x1 * x2)) , id +1)

      case _ => tree

    }
  }

  def simplifyAssociativeAndCommutative(tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3,id: Int): Tree3 = {
    tree match {
      case Multiply3(left, Multiply3(left1, Int3(x))) => //associative and commutative
        simplifyMore(Multiply3(Multiply3(left, left1), Int3(x)), id + 1)

      case _ => tree
    }
  }

  def simplifDivisionCancel (tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3,id:Int): Tree3 = {
    tree match {
      case Multiply3(Divide3(x1, y1), Divide3(x2, y2)) => {
        if (SimplifyRuleSets.equivalent(x1, y1)) simplifyMore(Divide3(x2, y2), id + 1)
        else if (SimplifyRuleSets.equivalent(x1, y2)) simplifyMore(Divide3(x2, y1), id + 1)
        else if (SimplifyRuleSets.equivalent(x2, y1)) simplifyMore(Divide3(x1, y2), id + 1)
        else if (SimplifyRuleSets.equivalent(x2, y2)) simplifyMore(Divide3(x1, y1), id + 1)
        else tree
      }
      case Multiply3(left, Divide3(x2, y2)) => {
        if (SimplifyRuleSets.equivalent(left, y2)) simplifyMore(x2, id + 1)
        else tree
      }
      case Multiply3(Divide3(x1, y1), right) => {
        if (SimplifyRuleSets.equivalent(y1, right)) simplifyMore(x1, id + 1)
        else tree
      }
      case _ => tree
    }
  }
}
