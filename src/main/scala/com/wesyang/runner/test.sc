import com.persist.uw.examples.Simplify._
import com.persist.uw.examples._

val t1 = Add3(Int3(-4), Int3(2))
val t2 = Subtract3(Int3(4), Int3(2))
val t3 = Multiply3(Int3(4), Int3(2))
val t4 = Divide3(Int3(4), Int3(2))
val t5 = Add3(Add3(t1, t3), Multiply3(t2, t4))

display(t5)
simplify(t1)

val e = Int3(5)
val e1 = Int3(5)
val e2 = Int3(6)

equivalent( e1, e2)
equivalent( e1, e)
equivalent( Name3("a"), Name3("a"))
equivalent( Name3("a"), Name3("b"))

equivalent( Add3(Int3(1),Name3("a")), Name3("b"))
equivalent( Add3(Int3(1),Name3("a")), Add3(Int3(1),Name3("b")))

equivalent( Add3(Name3("a"), Int3(1)), Add3(Int3(1),Name3("a")))
equivalent( Add3(Name3("a"), Name3("b")), Add3(Name3("b"),Name3("a")))

equivalent( Add3(Name3("a"), Int3(1)), Multiply3(Int3(1),Name3("b")))

val n1 = Add3 (Name3("a"), Int3(3))
simplify(n1)
val n11 = Add3 (Int3(8), Int3(3))
simplify(n11)

var n2 = Multiply3(Name3("a"), Int3(3))
simplify(n2)
var n23 = Multiply3(Int3(8), Int3(3))
simplify(n23)

var n3 = Subtract3(Int3(0) , Name3("b"))
simplify(n3)
var n33 = Subtract3(Int3(0) , Int3(9))
simplify(n33)

var n4 = Divide3(Name3("b") , Int3(-1))
simplify(n4)
var n43 = Divide3(Int3(6) , Int3(-1))
simplify(n43)

simplify(Divide3(n3, n4))



