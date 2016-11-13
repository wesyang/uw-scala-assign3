package com.persist.uw.examples

import org.specs2._

class TestTree4 extends mutable.Specification {

  import Tree4._
  import Tree4Extend._

   val t1 = If4(Add4(Int4(1), Int4(-1)), Name4("a"), Name4("b"))
   val t2 = Add4(Multiply4(Int4(2), Add4(Int4(3), Name4("c"))), Int4(4))

   "size" >> {
     (t1.size shouldEqual 6) and
       (t2.size shouldEqual 7)
   }

   "depth" >> {
     (t1.depth shouldEqual 3) and
       (t2.depth shouldEqual 4)
   }

 }
