package com.persist.uw.examples.TestSimplifyExtra

import org.specs2._
import com.persist.uw.examples._

class TestSimplifyExtraDist extends mutable.Specification {

  import Simplify.simplify

  "Testcase: (3 * a) + a == a * 4" >> {
    val a1 = Add3(Multiply3(Int3(3), Name3("a")), Name3("a"))
    simplify(a1) shouldEqual Multiply3( Name3("a"), Int3(4));
  }

  "Testcase: a + ( -33 * a)  == a * (-32)" >> {
    val a1 = Add3( Name3("a"), Multiply3(Int3(-33), Name3("a")))
    simplify(a1) shouldEqual Multiply3( Name3("a"), Int3(-32));
  }

  "Testcase: (a + b) + 2 * ( b + a )  ==  (a + b) * 3" >> {
    val a = Name3("a")
    val b = Name3("b")
    val e1 = Add3 (a, b)
    val e2 = Add3(b, a)
    val exp = Add3(e1, Multiply3(Int3(2), e2))
    val s =simplify(exp)
    s shouldEqual Multiply3 (e1, Int3(3))
  }

  "Testcase: (3 * a) - a == a * 2" >> {
    val a1 = Subtract3(Multiply3(Int3(3), Name3("a")), Name3("a"))
    simplify(a1) shouldEqual Multiply3( Name3("a"), Int3(2));
  }

  "Testcase: (3 * a) - (-5) * a == a * 8" >> {
    val a1 = Subtract3(Multiply3(Int3(3), Name3("a")), Multiply3(Int3(-5), Name3("a")))
    simplify(a1) shouldEqual Multiply3( Name3("a"), Int3(8));
  }

  "Testcase: a - ( -33 * a)  == a * 34" >> {
    val a1 = Subtract3( Name3("a"), Multiply3(Int3(-33), Name3("a")))
    simplify(a1) shouldEqual Multiply3( Name3("a"), Int3(34));
  }

  "Testcase: (a + 1 + b + 3) - 2   ==  4 + a + b" >> {
    val a = Name3("a")
    val b = Name3("b")
    val result = Add3(Int3(4) , Add3(a, b))

    val e1 = Add3 (a, Int3 (1))
    val e2 = Add3(e1, b)
    val e3 = Add3(e2, Int3(3))
    val r = simplify (e3)
    r shouldEqual result
  }

  "Testcase: (a + 1 + b + 3) - 2 * ( b + (2 *3 ) + a -2 )  ==  ( 4 + a + b) * (-1)" >> {
    val a = Name3("a")
    val b = Name3("b")
    val result = Add3(Int3(4) , Add3(a, b))

    val e1 = Add3 (a, Int3 (1))
    val e2 = Add3(e1, b)
    val e3 = Add3(e2, Int3(3))

    val x1 = Multiply3 (Int3(2), Int3(3))
    val x2 = Add3(b, x1)
    val x3 = Add3(x2, a)
    val x5 = Subtract3(x3, Int3(2))
    val x6 =  Multiply3(Int3(2), x5)

    val exp = Subtract3(e3,x6)
    val s =simplify(exp)
    s shouldEqual Multiply3 (result, Int3(-1))
  }

  "Testcase: ((((a + a) + a) + a) + a)   ==  a * 5" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Add3(Add3(a,a), a)
    val d = Add3(c, a)
    val e = Add3(d, a)

    val r = simplify (e)
    r shouldEqual Multiply3 (a, Int3(5))
  }

  "Testcase: (a + (a + (a + (a + a))))   ==  a * 5" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Add3(a, Add3(a,a))
    val d = Add3(a, c)
    val e = Add3(a, d)

    val r = simplify (e)
    r shouldEqual Multiply3 (a, Int3(5))
  }

  "Testcase:(((a + (a + a)) + a) + (a + a))   ==  a * 6" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Add3(a,a)
    val d = Add3(a, c)
    val e = Add3(d, a)
    val f = Add3(e, c)

    val r = simplify (f)
    r shouldEqual Multiply3 (a, Int3(6))
  }

  "Testcase:  (((a + a) + (a + a)) + (a + a))   ==  a * 6" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Add3(a,a)
    val d = Add3(c, c)
    val e = Add3(d, c)
    val r = simplify (e)
    r shouldEqual Multiply3 (a, Int3(6))
  }

  "Testcase: (((a + a) + (a + a)) + a)   ==  a * 5" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Add3(a,a)
    val d = Add3(c, c)
    val e = Add3(d, c)
    val f = Add3(d, a)
    val r = simplify (f)
    r shouldEqual Multiply3 (a, Int3(5))
  }

  "Testcase: (a + (((a + a) + (a + a)) + a))   ==  a * 5" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Add3(a,a)
    val d = Add3(c, c)
    val e = Add3(d, c)
    val f = Add3(d, a)
    val g = Add3 (a, f)
    val r = simplify (g)
    r shouldEqual Multiply3 (a, Int3(6))
  }

  "Testcase:((b * 3) * 3)   ==  (b * 9)" >> {
    val b = Name3("b")
    val c = Multiply3(b, Int3(3))
    val d = Multiply3(c, Int3(3))
    val r = simplify(d)
    r shouldEqual Multiply3 (b, Int3(9))
  }

  "Testcase:(3 * (3 * b))   ==  (b * 9)" >> {
    val b = Name3("b")
    val c = Multiply3(Int3(3), b)
    val d = Multiply3(Int3(3), c)
    val r = simplify(d)
    r shouldEqual Multiply3 (b, Int3(9))
  }

  "Testcase:((3 * b) * 3)   ==  (b * 9)" >> {
    val b = Name3("b")
    var a = Name3("a")
    val c = Multiply3(Int3(3), b)
    val d = Multiply3( c, Int3(3))
    val r = simplify(d)
    r shouldEqual Multiply3 (b, Int3(9))
  }

  "Testcase: ((3 * b) * (a * 3))  ==   ((b * a) * 9)" >> {
    val b = Name3("b")
    var a = Name3("a")
    val c = Multiply3(Int3(3), b)
    val d = Multiply3( a, Int3(3))
    val e = Multiply3(c, d)
    val r = simplify(e)
    r shouldEqual Multiply3 (Multiply3(b, a), Int3(9))
  }
}


