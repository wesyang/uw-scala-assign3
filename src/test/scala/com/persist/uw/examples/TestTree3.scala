package com.persist.uw.examples

import org.specs2._

class TestTree3 extends mutable.Specification {

  val t1 = If3(Add3(Int3(1), Int3(-1)), Name3("a"), Name3("b"))
  val t2 = Add3(Multiply3(Int3(2), Add3(Int3(3), Name3("c"))), Int3(4))

  "size" >> {
    (Tree3.size(t1) shouldEqual 6) and
      (Tree3.size(t2) shouldEqual 7)
  }

  "depth" >> {
    (Tree3.depth(t1) shouldEqual 3) and
      (Tree3.depth(t2) shouldEqual 4)
  }

}
