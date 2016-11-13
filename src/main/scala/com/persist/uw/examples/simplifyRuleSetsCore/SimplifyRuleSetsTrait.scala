package com.persist.uw.examples.simplifyRuleSetsCore

import com.persist.uw.examples.{Tree3, _}

object SimplifyRuleSets {

  /** *************************************************************************************************
    * Compare two Tree expressions are the same
    */
  def equivalent(left: Tree3, right: Tree3): Boolean = {
    (left, right) match {
      case (Name3(_), Int3(_)) => false
      case (Int3(_), Name3(_)) => false
      case (Int3(x), Int3(y)) => x == y
      case (Name3(x), Name3(y)) => x == y

      case (Add3(x1, y1), Add3(x2, y2)) => ((equivalent(x1, x2) && equivalent(y1, y2))) || ((equivalent(y1, x2) && equivalent(x1, y2)))
      case (Multiply3(x1, y1), Multiply3(x2, y2)) => ((equivalent(x1, x2) && equivalent(y1, y2))) || ((equivalent(y1, x2) && equivalent(x1, y2)))
      case (Divide3(x1, y1), Divide3(x2, y2)) => equivalent(x1, x2) && equivalent(y1, y2)
      case (Subtract3(x1, y1), Subtract3(x2, y2)) => equivalent(x1, x2) && equivalent(y1, y2)
      case _ => false
    }
  }
}

/**
  * Created by wesyang on 11/4/2016.
  */
trait SimplifyRuleSets {
  def buildRules() :  List[(Tree3, (Tree3, Int)=> Tree3, Int) => Tree3]
  def createTree (left: Tree3, right: Tree3): Tree3
  def splitTree (tree:Tree3): (Tree3,Tree3)

  def simplifyBothSide(tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3, id: Int): Tree3 = {
    splitTree (tree) match {
      case (left, right) => {
        val newLeft = simplifyMore(left, id + 1)
        val newRight = simplifyMore(right, id + 1)
        if ((newLeft == left) && (newRight == right))
          tree
        else
          simplifyMore(createTree(newLeft, newRight), id + 2)
      }
      case _  =>tree
    }
  }

  def simplifyByRules  (tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3, id: Int): Tree3 = {
    def executeRules(tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3, rules: List[(Tree3, (Tree3, Int)=> Tree3, Int)
      => Tree3], id: Int): Tree3 = {
      rules match {
        case firstRule +: restReules => {
          val newTree = firstRule(tree, simplifyMore, id)
          if (newTree != tree) newTree
          else executeRules(tree, simplifyMore, restReules, id)
        }
        case _ => tree
      }
    }

    // buildRules() is virtual function.  Every child class/object knows how to build the rules (patterns)
    // please review following *.scala file for Pattern match
    // SimplifyAddRules.scala for Add3 patterns;
    // SimplifySubstractRules.scala for Substract3 patterns,
    // SimplifyMultiplyRules.scala for Multiply3 patterns
    // SimplifyDivideRules.acala for Divide3 patterns

    val rules = buildRules()
    executeRules(tree, simplifyMore:(Tree3, Int)=> Tree3, rules, id)
  }
}
