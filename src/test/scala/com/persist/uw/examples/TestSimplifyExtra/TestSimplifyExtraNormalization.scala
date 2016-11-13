package com.persist.uw.examples.TestSimplifyExtra

import com.persist.uw.examples._
import org.specs2._

class TestSimplifyExtraNormalization extends mutable.Specification {

  import Simplify.simplify

  "Testcase: a + 3 normalize to  3 + a" >> {
    val n1 = Add3(Name3("a"), Int3(3))
    simplify(n1) shouldEqual Add3(Int3(3), Name3("a"));
  }

  "Testcase: 8 + 3 eval to  11" >> {
    val n11 = Add3(Int3(8), Int3(3))
    simplify(n11) shouldEqual Int3(11)
  }

  "Testcase: 3 * a normalize to a * 3" >> {
    val n2 = Multiply3(Int3(3), Name3("a") )
    val r2 = simplify(n2)
    val n23 = Multiply3(Int3(8), Int3(3))
    val r23 = simplify(n23)

    r2 shouldEqual (Multiply3(Name3("a"), Int3(3) ))
    r23 shouldEqual (Int3(24))
  }

  "Testcase: 0 - b normalize to b * (-1)" >> {
    val n3 = Subtract3(Int3(0), Name3("b"))
    val r3 = simplify(n3)
    val n33 = Subtract3(Int3(0), Int3(9))
    val r33 = simplify(n33)

    r3 shouldEqual (Multiply3(Name3("b"), Int3(-1) ))
    r33 shouldEqual (Int3(-9))
  }

  "Testcase: b / -1 normalize to b * (-1)" >> {
    val n4 = Divide3(Name3("b"), Int3(-1))
    val r4 =simplify(n4)
    val n43 = Divide3(Int3(6), Int3(-1))
    val r43 = simplify(n43)

    r4 shouldEqual (Multiply3(Name3("b"), Int3(-1) ))
    r43 shouldEqual (Int3(-6))
  }

  "Testcase: (0 - b) /  (b / -1) eval to  1" >> {
    val n3 = Subtract3(Int3(0), Name3("b"))
    val n4 = Divide3(Name3("b"), Int3(-1))
    simplify(Divide3(n3, n4)) shouldEqual Int3(1)
  }

  "Testcase:(b / (6 / a)) ==  ((b * a) / 6)" >> {
    val a = Name3("a")
    val b = Name3("b")
    val left = Divide3(Int3(6) ,a )
    val right = Divide3(b, left)
    val s =simplify(right)
    s shouldEqual( Divide3 (Multiply3(b, a), Int3(6)))
  }
}

