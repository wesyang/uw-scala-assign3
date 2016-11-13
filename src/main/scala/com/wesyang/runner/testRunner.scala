package com.wesyang.runner

import com.persist.uw.examples.Simplify._
import com.persist.uw.examples._
import com.persist.uw.examples.simplifyRuleSetsCore.ArithmeticHelper

/**
  * Created by wesyang on 11/1/2016.
  */
object testRunner {

  def TestSimple(): Unit = {
    val t1 = Add3(Int3(4), Int3(2))
    val t2 = Subtract3(Int3(4), Int3(2))
    val t3 = Multiply3(Int3(4), Int3(2))
    val t4 = Divide3(Int3(4), Int3(2))
    val t5 = Add3(Add3(t1, t3), Multiply3(t2, t4))

    display(t5)
    simplify(t1)
    simplify(t2)
    simplify(t3)
    simplify(t4)
    simplify(Add3(t1, t3))
    simplify(Multiply3(t2, t4))
    simplify(t5)
  }

  def TestSubtract(): Unit = {
    val t1 = Divide3(Name3("a"), Name3("b"))
    val t2 = Divide3(Name3("a"), Add3(Name3("b"), Int3(0)))
    val t3 = Subtract3(t1, t2)
    display(t3)


    val t4 = Add3(Name3("b"), Int3(0))
    val t5 = Add3(Int3(0), Name3("b"))
    simplify(t4)
    simplify(t5)

    val t6 = Multiply3(Name3("b"), Int3(1))
    val t7 = Multiply3(Int3(1), Name3("b"))
    simplify(t6)
    simplify(t7)

    val t8 = Multiply3(Name3("b"), Int3(0))
    val t9 = Multiply3(Int3(0), Name3("b"))
    simplify(t8)
    simplify(t9)

    val t10 = Divide3(Name3("b"), Int3(1))
    val t11 = Divide3(Int3(0), Name3("b"))
    simplify(t10)
    simplify(t11)

    val diff = equivalent (Add3(Int3(1), Name3("a")), Add3(Int3(1), Name3("b")))
    println("1 + a" == "1 + b: " + diff.toString())


    simplify(t3)
  }

  def TestIf(): Unit = {
    val t1 = If3(Int3(0), Name3("a"), Name3("b"))
    val t2 = If3(Int3(1), Name3("a"), Name3("b"))
    simplify(t1)
    simplify(t2)
  }

  def TestNormalizeExpress(): Unit = {
    val n1 = Add3(Name3("a"), Int3(3))
    simplify(n1)
    val n11 = Add3(Int3(8), Int3(3))
    simplify(n11)

    val n2 = Multiply3(Name3("a"), Int3(3))
    simplify(n2)
    val n23 = Multiply3(Int3(8), Int3(3))
    simplify(n23)

    val n3 = Subtract3(Int3(0), Name3("b"))
    simplify(n3)
    val n33 = Subtract3(Int3(0), Int3(9))
    simplify(n33)

    val n4 = Divide3(Name3("b"), Int3(-1))
    simplify(n4)
    val n43 = Divide3(Int3(6), Int3(-1))
    simplify(n43)

    simplify(Divide3(n3, n4))
  }

  def TestAssociateDist(): Unit = {
    val a1 = Add3(Add3( Name3("a"), Int3(9)),Int3(3))
    simplify(a1);

    val a2 = Add3(Add3( Name3("a"), Int3(9)), Add3( Name3("b"),Int3(3)) )
    simplify(a2);

    val a3 = Subtract3( Name3("a"), Int3(9))
    simplify(a3)
    simplify(Add3(a3,a3))
  }

  def main(args: Array[String]): Unit = {
    println("hello world")
    //TestSimple
    //TestSubtract
   // TestIf
    //TestNormalizeExpress
    //TestAssociateDist

    //val list = ArithmeticHelper.getPrimes(1000)
    //println (list)
    //println (ArithmeticHelper.getCommonDenominator (6*2*5, 9*5))

    /*

         case Subtract3(Int3(x), Add3(Int3(y), right)) => //associative and commutative
            doSimplify(Subtract3(Int3(x - y), right), id + 1)
          case Subtract3(Int3(x), Subtract3(Int3(y), right)) => //associative and commutative
            doSimplify(Add3(Int3(x - y), right), id + 1)

          case Subtract3(Add3(Int3(x), left), Add3(Int3(y), right)) => //associative and commutative
            doSimplify(Add3(Int3(x - y), Subtract3(left, right)), id + 1)
          case Subtract3(Add3(Int3(x), left), Subtract3(Int3(y), right)) => //associative and commutative
            doSimplify(Add3(Int3(x - y), Add3(left, right)), id + 1)
          case Subtract3(Subtract3(Int3(x), left), Add3(Int3(y), right)) => //associative and commutative
            doSimplify(Subtract3(Int3(x - y), Add3(right, left)), id + 1)
          case Subtract3(Subtract3(Int3(x), left), Subtract3(Int3(y), right)) => //associative and commutative
            doSimplify(Add3(Int3(x - y), Subtract3(right, left)), id + 1)

          case Subtract3(Subtract3(Int3(x), right1), right) => //associative and commutative
            doSimplify(Subtract3(Int3(x), Add3(right1, right)), id + 1)
          case Subtract3(Add3(Int3(x), right1), right) => //associative and commutative
            doSimplify(Add3(Int3(x), Subtract3(right1, right)), id + 1)
     */

/*
          case Add3(Subtract3(Int3(x), right1), right) => //associative and commutative
            doSimplify(Add3(Int3(x), Subtract3(right, right1)), id + 1)
 */


    /*

    val t1 = Divide3(Name3("a"), Name3("b"))
    val t2 = Divide3(Name3("a"), Add3(Name3("b"), Int3(0)))
    simplify(Divide3(t1, t2))

  */

    val a = Name3("a")
    val b = Name3("b")
    val t = Multiply3 (Int3 (2*3*5*7), a)
    val l = Multiply3 (Int3(3*7*9), b)
    val s = Divide3 (t, l)

    val x =  Multiply3 (Int3 (3*8), a)
    val y =  Multiply3 (Int3 (3*9), b)
    val w = Divide3 (x, y)
    val k = Add3 (s , w)

    val r = simplify(k)
    //val result = equivalent(t, tt)
    //println (result)
   //


    //

    println("done")
  }

}
