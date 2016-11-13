package com.persist.uw.examples.TestSimplifyExtra

/**
  * Created by wesyang on 11/2/2016.
  */

import org.specs2._
import com.persist.uw.examples._

class TestSimplifyExtraAssociative extends mutable.Specification {

  import Simplify.simplify

  "Testcase: (a + 9) + 3 == 12 + a" >> {
    val a1 = Add3(Add3(Name3("a"), Int3(9)), Int3(3))
    simplify(a1) shouldEqual Add3(Int3(12), Name3("a"));
  }

  "Testcase: (a + 9) + (b +3) == 12 + (a +b)" >> {
    val a2 = Add3(Add3(Name3("a"), Int3(9)), Add3(Name3("b"), Int3(3)))
    simplify(a2) shouldEqual Add3(Int3(12), Add3(Name3("a"), Name3("b")));
  }

  "Testcase: (a - 9) + (b - 3) == -12 + (a + b)" >> {
    val a3 = Subtract3(Name3("a"), Int3(9))
    val b3 = Subtract3(Name3("b"), Int3(3))
    val r = simplify(Add3(a3, b3))

    r shouldEqual Add3(Int3(-12), Add3(Name3("a"), Name3("b")))
  }

  "Testcase: (a - 9) + (a - 3) == -12 + 2 * a" >> {
    val a3 = Subtract3(Name3("a"), Int3(9))
    val b3 = Subtract3(Name3("a"), Int3(3))
    val r = simplify(Add3(a3, b3))

    r shouldEqual Add3(Int3(-12), Multiply3(Name3("a"), Int3(2)))
  }

  "Testcase: (a + 1 + b + 3) - 2   ==  4 + a + b" >> {
    val a = Name3("a")
    val b = Name3("b")
    val result = Add3(Int3(4), Add3(a, b))

    val e1 = Add3(a, Int3(1))
    val e2 = Add3(e1, b)
    val e3 = Add3(e2, Int3(3))
    val r = simplify(e3)
    r shouldEqual result
  }

  "Testcase: 2 * ( b + (2 *3 ) + a -2 )  ==  (4 + b + a) * 2" >> {
    val a = Name3("a")
    val b = Name3("b")
    val result = Add3(Int3(4), Add3(b, a))

    val x1 = Multiply3(Int3(2), Int3(3))
    val x2 = Add3(b, x1)
    val x3 = Add3(x2, a)
    val x5 = Subtract3(x3, Int3(2))
    val x6 = Multiply3(Int3(2), x5)
    simplify(x6) shouldEqual (Multiply3(result, Int3(2)))
  }


}