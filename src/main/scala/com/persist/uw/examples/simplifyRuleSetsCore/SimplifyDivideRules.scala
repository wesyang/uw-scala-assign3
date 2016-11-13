package com.persist.uw.examples.simplifyRuleSetsCore

import com.persist.uw.examples._
import sun.management.jmxremote.ConnectorBootstrap.DefaultValues

/**
  * Created by wesyang on 11/4/2016.
  */
object SimplifyDivideRules extends SimplifyRuleSets {

  override def createTree(left: Tree3, right: Tree3): Tree3 =  {Divide3(left, right)}

  override def splitTree(tree: Tree3): (Tree3, Tree3) =  {
    tree match {
      case Divide3(left, right) => (left, right)
      case _ => throw new IllegalArgumentException("tree is not Divide3 node")
    }
  }

  /** ************************************************************************************************
    * return set of functions.
    * each function contains the rules to simplify Divide3 expression
    */
  override def buildRules(): List[(Tree3, (Tree3, Int)=> Tree3, Int) => Tree3] = {
    List[(Tree3, (Tree3, Int)=> Tree3, Int) => Tree3](
      simplifyBasic,
      simplifyEquivalent,
      simplifDivisionCancel,
      simplifyBothSide,
      simplifyByTransform
    )
  }

  def simplifyBasic (tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3, id:Int): Tree3 = {
    tree match {
      case Divide3(Int3(0), right) => Int3(0)
      case Divide3(right, Int3(0)) => throw new IllegalArgumentException("can not divid by zero")
      case Divide3(left, Int3(1)) => simplifyMore(left, id + 1)

      case Divide3(Int3(x), Int3(y)) =>  {
        if (ArithmeticHelper.absoluteValue(x) % ArithmeticHelper.absoluteValue(y) ==0 ) Int3(x/y)
        else {
          val cd = ArithmeticHelper.getCommonDenominator(x, y)
          Divide3( Int3(x/cd), Int3(y/cd))
        }
      }

      case Divide3(Int3(x), Name3(y)) => tree
      case Divide3(left, Int3(-1)) => simplifyMore(Multiply3(left, Int3(-1)), id + 1) // normalize
      case Divide3(Name3(y), Int3(x)) => tree

      case _ => tree
    }
  }

  def simplifyEquivalent(tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3,id: Int): Tree3 = {
    tree match {
      case Divide3(left, right) =>
        if (SimplifyRuleSets.equivalent(left, right)) Int3(1)
        else tree
      case _ => tree
    }
  }

  def simplifDivisionCancel(tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3,id: Int): Tree3 = {
    tree match {
      case Divide3(Multiply3(x1, y1), Multiply3(x2, y2)) => {
        if (SimplifyRuleSets.equivalent(x1, x2)) simplifyMore(Divide3(y1, y2), id + 1)
        else if (SimplifyRuleSets.equivalent(x1, y2)) simplifyMore(Divide3(y1, x2), id + 1)
        else if (SimplifyRuleSets.equivalent(y1, x2)) simplifyMore(Divide3(x1, y2), id + 1)
        else if (SimplifyRuleSets.equivalent(y1, y2)) simplifyMore(Divide3(x1, x2), id + 1)
        else tree
      }
      case Divide3(left, Multiply3(x2, y2)) => {
        if (SimplifyRuleSets.equivalent(left, x2)) simplifyMore(Divide3(Int3(1), y2), id + 1)
        else if (SimplifyRuleSets.equivalent(left, y2)) simplifyMore(Divide3(Int3(1), x2), id + 1)
        else tree
      }
      case Divide3(Multiply3(x1, y1), right) => {
        if (SimplifyRuleSets.equivalent(x1, right)) simplifyMore(y1, id + 1)
        else if (SimplifyRuleSets.equivalent(y1, right)) simplifyMore(x1, id + 1)
        else tree
      }
      case _ => tree
    }
  }

  def simplifyByTransform(tree: Tree3, simplifyMore:(Tree3, Int)=> Tree3,id: Int): Tree3 = {
    tree match {
        // b / (a /c ) --> (b * c) / a
      case Divide3(left, Divide3(left2, right2)) => simplifyMore(Divide3(Multiply3(left, right2), left2), id + 1) // normalize
      case Divide3(Multiply3(x1, Int3(y1)), Multiply3(x2, Int3(y2))) =>  // ( a * 3) / ( (b * 2) -> (a/b) * (3/2)
        simplifyMore(Multiply3 (Divide3(x1, x2), Divide3(Int3(y1), Int3(y2))), id + 1)
      case _ => tree
    }
  }
}