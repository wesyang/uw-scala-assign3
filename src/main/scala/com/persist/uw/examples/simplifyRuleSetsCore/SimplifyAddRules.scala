package com.persist.uw.examples.simplifyRuleSetsCore
import com.persist.uw.examples._

/**
  * Created by wesyang on 11/4/2016.
  */
object SimplifyAddRules extends SimplifyRuleSets {


  override def createTree(left: Tree3, right: Tree3): Tree3 = {
    Add3(left, right)
  }

  override def splitTree(tree: Tree3): (Tree3, Tree3) = {
    tree match {
      case Add3(left, right) => (left, right)
      case _ => throw new IllegalArgumentException("tree is not Add3 node")
    }
  }

  /** ************************************************************************************************
    * return set of functions.
    * each function contains the rules to simplify Add3 expression
    */

  override def buildRules(): List[(Tree3, (Tree3, Int) => Tree3, Int) => Tree3] = {
    List[(Tree3, (Tree3, Int) => Tree3, Int) => Tree3](
      simplifyBasic,
      simplifyEquivalent,
      simplifyBothSide,
      simplifyDistributive,
      simplifyAssociativeAndCommutative
    )
  }

  def simplifyBasic(tree: Tree3, simplifyMore: (Tree3, Int) => Tree3, id: Int): Tree3 = {
    tree match {
      case Add3(Int3(0), right) => simplifyMore(right, id + 1) // 0 + any -> x
      case Add3(left, Int3(0)) => simplifyMore(left, id + 1) // any + 0 -> any
      case Add3(Int3(x), Int3(y)) => Int3(x + y) // 1 + 2 -> 3

      case Add3(Int3(_), Name3(_)) => tree  // 1 + x --> un-chnage
      case Add3(left, Int3(x)) => simplifyMore(Add3(Int3(x), left), id + 1) // normalize  x + 1  -> 1 + x

      case _ => tree
    }
  }

  def simplifyEquivalent(tree: Tree3, simplifyMore: (Tree3, Int) => Tree3, id: Int): Tree3 = {
    tree match {
      case Add3(left, right) =>

        // a + a -> 2 * a
        if (SimplifyRuleSets.equivalent(left, right)) simplifyMore(Multiply3(left, Int3(2)), id + 1)
        else tree
      case _ => tree
    }
  }

  def simplifyDistributive(tree: Tree3, simplifyMore: (Tree3, Int) => Tree3, id: Int): Tree3 = {
    def simplifyDistAdd(tree: Tree3, c1: Int, x1: Tree3, c2: Int, x2: Tree3, id: Int): Tree3 = {
      if (SimplifyRuleSets.equivalent(x1, x2)) simplifyMore(Multiply3(x1, Int3(c1 + c2)), id + 1)
      else tree
    }

    tree match {
      case Add3(Multiply3(left1, Int3(x1)), Multiply3(left2, Int3(x2))) => // distributive
        simplifyDistAdd(tree, x1, left1, x2, left2, id)
      case Add3(left, Multiply3(left2, Int3(x2))) => //distributive
        simplifyDistAdd(tree, 1, left, x2, left2, id)
      case Add3(Multiply3(left1, Int3(x1)), right) => //distributive
        simplifyDistAdd(tree, x1, left1, 1, right, id)

      case Add3(Multiply3(left1, right1), Multiply3(left2, right2)) => // distributive
        if (SimplifyRuleSets.equivalent(left1, left2))  simplifyMore(Multiply3(left1, Add3(right1, right2)), id +1)
        else if (SimplifyRuleSets.equivalent(left1, right2))  simplifyMore(Multiply3(left1, Add3(right1, left2)), id +1)
        else if (SimplifyRuleSets.equivalent(right1, left2))  simplifyMore(Multiply3(right1, Add3(left1, right2)), id +1)
        else if (SimplifyRuleSets.equivalent(right1, right2))  simplifyMore(Multiply3(right1, Add3(left1, left2)), id +1)
        else tree

      case Add3(Divide3(left1, right1), Divide3(left2, right2)) => // distributive
        if (SimplifyRuleSets.equivalent(right1, right2))  simplifyMore(Divide3(Add3(left1, left2), right1), id +1)
        else tree
      case _ => tree
    }
  }

  def simplifyAssociativeAndCommutative(tree: Tree3, simplifyMore: (Tree3, Int) => Tree3, id: Int): Tree3 = {
    tree match {
      case Add3(Int3(x), Add3(Int3(y), right)) => // 1 + (3 + any) -> 4 + any
        simplifyMore(Add3(Int3(x + y), right), id + 1)
      case Add3(Int3(x), Subtract3(Int3(y), right)) => // 1 + (3 - any) -> 4 - any
        simplifyMore(Subtract3(Int3(x + y), right), id + 1)

      case Add3(Add3(Int3(x), left), Add3(Int3(y), right)) => // (1 + left) + (3 + right) -> 4 + (left + right)
        simplifyMore(Add3(Int3(x + y), Add3(left, right)), id + 1)
      case Add3(Add3(Int3(x), left), Subtract3(Int3(y), right)) => // (1 + left) + (3 - right) -> 4 + (left - right)
        simplifyMore(Add3(Int3(x + y), Subtract3(left, right)), id + 1)
      case Add3(Subtract3(Int3(x), left), Add3(Int3(y), right)) => // (1 - left) + (3 + right) -> 4 + (right - left)
        simplifyMore(Add3(Int3(x + y), Subtract3(right, left)), id + 1)
      case Add3(Subtract3(Int3(x), left), Subtract3(Int3(y), right)) => // (1 - left) + (3 - right) -> 4 - (right + left)
        simplifyMore(Subtract3(Int3(x + y), Add3(right, left)), id + 1)

      case Add3(Add3(Int3(x), right1), right) =>  //( 1 + x1) + y1 -> 1 + (x1 + y1)
        simplifyMore(Add3(Int3(x), Add3(right1, right)), id + 1)
      case Add3(Subtract3(Int3(x), right1), right) => //( 1 - x1) + y1 -> 1 + (y1 - x1)
        simplifyMore(Add3(Int3(x), Subtract3(right, right1)), id + 1)

      case _ => tree
    }
  }
}
