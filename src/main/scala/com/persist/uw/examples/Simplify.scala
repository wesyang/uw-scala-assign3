package com.persist.uw.examples

import com.google.common.util.concurrent.CycleDetectingLockFactory.WithExplicitOrdering
import com.persist.uw.examples.simplifyRuleSetsCore._

object Simplify {

  /** *************************************************************************************************
    * Display functions.
    * Make Tree expression more readable
    */
  def getDisplayString(tree: Tree3): String = {
    tree match {
      case Int3(x) => if (x >= 0) x.toString() else "(" + x.toString() + ")"
      case Name3(y) => y
      case Add3(left, right) => "(" + getDisplayString(left) + " + " + getDisplayString(right) + ")"
      case Subtract3(left, right) => "(" + getDisplayString(left) + " - " + getDisplayString(right) + ")"
      case Multiply3(left, right) => "(" + getDisplayString(left) + " * " + getDisplayString(right) + ")"
      case Divide3(left, right) => "(" + getDisplayString(left) + " / " + getDisplayString(right) + ")"
      case If3(test, t, f) => "if (" + getDisplayString(test) + ") " + getDisplayString(t) + " else " + getDisplayString(f)
    }
  }

  def display(t: Tree3) = {
    println(getDisplayString(t))
  }

  def debug_print(msg: String, tree: Tree3, indent: Int) = {
    def getIndent(i: Int): String = {
      i match {
        case 0 => "      "
        case _ => "  " + getIndent(i - 1)
      }
    }
    println(getIndent(indent) + "step (" + indent.toString() + ") " + msg + getDisplayString(tree))
  }

  /** *************************************************************************************************
    * Simplify Tree expression
    *
    * use pattern matching
    * no vars or mutable data
    * pass all tests
    *
    * Math equation simplification is done by using math properties; such as Distributive,
    * Associative, Commutative, Normalization, Reduction and etc
    *
    * All simplification / reduction rules (patterns) are defined under package
    * com.persist.uw.examples.SimlifyRuleSetsCore (which is the child folder of the current folder)
    *
    * Additional Tests are defined under the package
    * package com.persist.uw.examples.TestSimplifyExtra (which is under the normal test folder)
    *
    */

  def simplify(tree: Tree3): Tree3 = {

    def doSimplify(tree: Tree3, id: Int): Tree3 = {

      debug_print("input expression: ", tree, id)

      val newTree = tree match {
        /* simple case */
        case Int3(_) | Name3(_) => tree

        case Add3(_, _) => SimplifyAddRules.simplifyByRules(tree, doSimplify, id);
        case Subtract3(_, _) => SimplifySubstractRules.simplifyByRules(tree, doSimplify, id);
        case Multiply3(_, _) => SimplifyMultiplyRules.simplifyByRules(tree, doSimplify, id);
        case Divide3(_, _) => SimplifyDivideRules.simplifyByRules(tree, doSimplify, id);

        case If3(conditionExpr, trueExpr, falseExpr) => {
          val condition = doSimplify(conditionExpr, id + 1)
          condition match {
            case Int3(0) => doSimplify(falseExpr, id + 2)
            case Int3(_) => doSimplify(trueExpr, id + 2)
            case _ => If3(condition, doSimplify(trueExpr, id + 2), doSimplify(falseExpr, id + 2))
          }
        }
        case _ => tree
      }

      debug_print("output expression: ", newTree, id)

      newTree
    }

    // function body
    doSimplify(tree, 0)

    // (todo) additional step to further simplify the equation is to performring sorting then
    // re evaluate the "simplify".   for example
    //  a + b + c + a + c,  currently, my pattern match is not able to simplify this
    //  equation.   However, If I perform the sorting and convert the equation to
    //  a + a + b + c + c,  then, my pattern match in "simplify" function can reduce it to
    // (a * 2) + b + (c *2).

  }

  /** *************************************************************************************************
    * Compare two Tree expressions are the same
    */
  def equivalent(left: Tree3, right: Tree3): Boolean = {
    simplifyRuleSetsCore.SimplifyRuleSets.equivalent(left, right)
  }
}

