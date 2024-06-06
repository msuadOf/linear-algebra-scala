import sfraction._
import sfraction.HasFraction._
import scala.math.Numeric.Implicits._

import linearAlgebra._
import linearAlgebra.utils._
import linearAlgebra.Implicits._

import scala.util.Random

case class test_660() extends Test("660") {
  val t1 = 0
  val t2 = 1

  val a = Array
    .tabulate(5, 5) { (i, j) =>
      if (i == j) t1 else if (i == j + 1) t2 else 0
    }
    .map(_.map(_.F))
  val m = Matrix(a)
  val p = m.update(0, m.numCols - 1, t2.F)
  println(p.toSquareMatrix)
  println(p.toSquareMatrix.det)
  // val m=Matrix(

  // )
}
