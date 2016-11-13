package com.persist.uw.examples

import org.specs2._

class TestTree1 extends mutable.Specification {

  val t1 = If1(Add1(Int1(1), Int1(-1)), Name1("a"), Name1("b"))
  val t2 = Add1(Multiply1(Int1(2), Add1(Int1(3), Name1("c"))), Int1(4))

  "size" >> {
    (t1.size shouldEqual 6) and
      (t2.size shouldEqual 7)
  }

  "depth" >> {
    (t1.depth shouldEqual 3) and
      (t2.depth shouldEqual 4)
  }

}
