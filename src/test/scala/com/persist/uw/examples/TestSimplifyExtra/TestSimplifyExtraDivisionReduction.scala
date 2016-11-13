package com.persist.uw.examples.TestSimplifyExtra

import com.persist.uw.examples._
import org.specs2.mutable

/**
  * Created by wesyang on 11/3/2016.
  */
class TestSimplifyExtraDivisionReduction extends mutable.Specification {

  import Simplify.simplify

  "Testcase:((b * a) / (b * 3)) ==  (a / 3)" >> {
    val a = Name3("a")
    val b = Name3("b")
    val top = Multiply3(b ,a )
    val bottom = Multiply3(b, Int3(3))
    val expr = Divide3(top, bottom)
    val r =simplify(expr)
    r shouldEqual( Divide3 (a, Int3(3)))
  }

  "Testcase:((b * a) / b) ==  a" >> {
    val a = Name3("a")
    val b = Name3("b")
    val top = Multiply3(b ,a )
    val bottom = b
    val expr = Divide3(top, bottom)
    val r =simplify(expr)
    r shouldEqual(a)
  }

  "Testcase:(a / (b * a)) ==  (1 / b)" >> {
    val a = Name3("a")
    val b = Name3("b")
    val top = Multiply3(b ,a )
    val expr = Divide3(a, top)
    val r =simplify(expr)
    r shouldEqual( Divide3(Int3(1), b))
  }

  "Testcase: (a / (a / b)) ==  b" >> {
    val a = Name3("a")
    val b = Name3("b")
    val top = Divide3(a ,b )
    val expr = Divide3(a, top)
    val r =simplify(expr)
    r shouldEqual( b)
  }

  "Testcase: ((210 * a) / (189 * b)) == ((a / b) * (10 / 9))" >> {
    val a = Name3("a")
    val b = Name3("b")
    val t = Multiply3 (Int3 (2*3*5*7), a)
    val l = Multiply3 (Int3(3*7*9), b)
    val s = Divide3 (t, l)
    val r = simplify(s)
    r shouldEqual( Multiply3( Divide3(a, b), Divide3(Int3(10), Int3(9))))
  }

  "Testcase: (((210 * a) / (189 * b)) + ((24 * a) / (27 * b))) ==  ((a / b) * 2)" >> {
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
    r shouldEqual( Multiply3( Divide3(a, b), Int3(2)))
  }
}