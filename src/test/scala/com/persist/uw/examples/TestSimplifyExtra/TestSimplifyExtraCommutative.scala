package com.persist.uw.examples.TestSimplifyExtra

import com.persist.uw.examples.Simplify._
import com.persist.uw.examples._
import org.specs2.mutable

/**
  * Created by wyang on 11/3/2016.
  */
class TestSimplifyExtraCommutative extends mutable.Specification {

  import Simplify.simplify

  "Testcase: 4 + (a + 6 ) == 10 + a" >> {
    val a = Name3("a")
    val b = Name3("b")
    val expr = Add3(Int3(4) , Add3(b, Int3(6)))
    val s =simplify(expr)
    s shouldEqual Add3(Int3(10), b)
  }
  "Testcase: 4 - (b + 6 ) == 10 - b" >> {
    val a = Name3("a")
    val b = Name3("b")
    val expr = Add3(Int3(4), Subtract3(Int3(6), b))
    val s = simplify(expr)
    s shouldEqual Subtract3(Int3(10), b)
  }

  "Testcase: 6 - (6 -a) == - a" >> {
    val a = Name3("a")
    val b = Name3("b")
    val expr = Add3(Int3(6), Subtract3(b, Int3(6)))
    val s = simplify(expr)
    s shouldEqual b
  }

  "Testcase: ((6 + a) + (b + (-6))) == (a + b)" >> {
    val a = Name3("a")
    val b = Name3("b")
    val left = Add3(Int3(6), a)
    val right = Add3(b, Int3(-6))
    val expr = Add3(left, right)
    val s = simplify(expr)
    s shouldEqual Add3( a, b)
  }
  "Testcase:((6 + a) + (a - 6) == a * 2" >> {
    val a = Name3("a")
    val b = Name3("b")
    val left = Add3(Int3(6), a)
    val right = Subtract3(a, Int3(6))
    val expr = Add3(left, right)
    val s = simplify(expr)
    s shouldEqual Multiply3( a,  Int3(2))
  }

  "Testcase: ((6 - a) + (a + (-6))) == 0" >> {
    val a = Name3("a")
    val b = Name3("b")
    val left = Subtract3(Int3(6) ,a )
    val right = Add3( a, Int3(-6))
    val expr = Add3(left, right)
    val s =simplify(expr)
    s shouldEqual Int3(0)
  }

  "Testcase: (5 + (1 + (6 + a))) == (12 + a)" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Add3(Int3(6) ,a )
    val right = Add3(Int3(1) ,c )
    val left = Add3( Int3(5), right)
    val s =simplify(left)
    s shouldEqual Add3( Int3(12), a)
  }

  "Testcase:  (((a + 6) + 1) + 5) == (12 + a)" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Add3( a, Int3(6) )
    val right = Add3(c, Int3(1)  )
    val left = Add3( right, Int3(5) )
    val s =simplify(left)
    s shouldEqual Add3( Int3(12), a)
  }

  "Testcase:  ((5 + (6 + a)) + b) ==  (11 + (a + b))" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Add3( Int3(6), a )
    val right = Add3(Int3(5), c  )
    val left = Add3(right, b  )
    val s =simplify(left)
    s shouldEqual Add3( Int3(11), Add3(a,b))
  }

  "Testcase:  (5 + ((a -6 ) - 1)) ==  ((-2) + a)" >> {
    val a = Name3("a")
    val b = Name3("b")
    val c = Subtract3( a, Int3(6))
    val right = Subtract3(c, Int3(1)  )
    val left = Add3( Int3(5), right  )
    val s =simplify(left)
    s shouldEqual Add3(Int3(-2),a)
  }


  "Testcase: ((6 - a) + ( -6 -a )) ==  a * -2" >> {
    val a = Name3("a")
    val b = Name3("b")
    val left = Subtract3(Int3(6) ,a )
    val right = Subtract3(Int3(-6), a)
    val expr = Add3(left, right)
    val s =simplify(expr)
    s shouldEqual Multiply3( a, Int3 (-2))
  }

  "Testcase: ((a * (3 * b)) * 5) ==  ((a * b) * 15)" >> {
    val b = Name3("b")
    var a = Name3("a")
    val c = Multiply3(Int3(3), b)
    val d = Multiply3( a,c)
    val e = Multiply3(d, Int3(5))
    val r = simplify(e)
    r shouldEqual Multiply3(Multiply3(a, b) , Int3 (15))
  }
}
