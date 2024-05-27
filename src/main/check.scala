trait HasCheck {
  import sfraction._
  import sfraction.HasFraction._
  import scala.math.Numeric.Implicits._

  import linearAlgebra._
  import linearAlgebra.Implicits._

  def checkonce(t: (Any, Any)): Boolean = {
    // println(t)
    // println(t._1 == t._2)

    t match {
  case (f1: Fraction, f2: Fraction) =>
    if (f1 == f2) {
      println("[-]")
      true
    } else {
      println("[x]" + f1)
      false
    }
  case (m1: Matrix, m2: Matrix) =>
    if (m1 == m2) {
      println("[-]")
      true
    } else {
      println("[x]" + m1)
      false
    }
  case (any1, any2) =>
    // 这里尝试通用比较，但请注意这可能引发 ClassCastException 如果 any1 和 any2 不可比较
    try {
      if (any1==any2) {
        println("[-]")
        true
      } else {
        println(s"[x] ${any1} != ${any2}")
        false
      }
    } catch {
      case e: ClassCastException =>
        println(s"[x] Unsupported comparison due to type mismatch: ${e.getMessage}")
        false
    }
}

  }
  def check(l: List[(Any, Any)]) = {
    val s   = l.map(checkonce(_))
    val res = s.reduce(_ && _)
    if (res) println("Ok") else println("Wrong")
    res
  }
}
