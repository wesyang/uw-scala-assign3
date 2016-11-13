package com.persist.uw.examples

import org.specs2._

class TestSimplify extends mutable.Specification {

  import Simplify.simplify

  "eval" >> {
    val t1 = Add3(Int3(4), Int3(2))
    val t2 = Subtract3(Int3(4), Int3(2))
    val t3 = Multiply3(Int3(4), Int3(2))
    val t4 = Divide3(Int3(4), Int3(2))
    val t5 = Add3(Add3(t1, t3), Multiply3(t2, t4))
    simplify(t5) shouldEqual Int3(18)
  }


  "subtractEqual" >> {
    val t1 = Divide3(Name3("a"), Name3("b"))
    val t2 = Divide3(Name3("a"), Add3(Name3("b"), Int3(0)))
    simplify(Subtract3(t1, t2)) shouldEqual Int3(0)
  }

  "divideEqual" >> {
    val t1 = Divide3(Name3("a"), Name3("b"))
    val t2 = Divide3(Name3("a"), Add3(Name3("b"), Int3(0)))
    simplify(Divide3(t1, t2)) shouldEqual Int3(1)

  }


  "testIf" >> {
    val t1 = If3(Int3(0), Name3("a"), Name3("b"))
    val t2 = If3(Int3(1), Name3("a"), Name3("b"))
    (simplify(t1) shouldEqual Name3("b")) and
      (simplify(t2) shouldEqual Name3("a"))
  }

  "testDist+" >> {
    val t1 = Add3(Multiply3(Name3("a"), Int3(5)), Multiply3(Name3("a"), Int3(3)))
    val t2 = Multiply3(Name3("a"), Int3(8))
    simplify(t1) shouldEqual t2
  }

  "testDist-" >> {
    val t1 = Subtract3(Multiply3(Name3("a"), Int3(5)), Multiply3(Name3("a"), Int3(3)))
    val t2 = Multiply3(Name3("a"), Int3(2))
    simplify(t1) shouldEqual t2
  }

  "add0" >> {
    val t1 = Add3(Name3("b"), Int3(0))
    val t2 = Add3(Int3(0), Name3("b"))
    (simplify(t1) shouldEqual Name3("b")) and
      (simplify(t2) shouldEqual Name3("b"))
  }
  "multiply1" >> {
    val t1 = Multiply3(Name3("b"), Int3(1))
    val t2 = Multiply3(Int3(1), Name3("b"))
    (simplify(t1) shouldEqual Name3("b")) and
      (simplify(t2) shouldEqual Name3("b"))
  }

  "multiply0" >> {
    val t1 = Multiply3(Name3("b"), Int3(0))
    val t2 = Multiply3(Int3(0), Name3("b"))
    (simplify(t1) shouldEqual Int3(0)) and
      (simplify(t2) shouldEqual Int3(0))
  }

}
