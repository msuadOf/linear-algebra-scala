import sfraction._
import sfraction.HasFraction._
import scala.math.Numeric.Implicits._

import linearAlgebra._
import linearAlgebra.utils._
import linearAlgebra.Implicits._
object Main extends App with HasCheck {
  val Fraction_show_checkList = List(
    (1.F).toString -> "1",
    (-2.F).toString -> "-2",
    (1.F / 2.F).toString -> "(1/2)",
    ("-1/2".F == "1/-2".F) -> true,
    ("-1/2".F == "-1/2".F) -> true,
    ("-1/2".F == "1/2".F) -> false,
    ("1/2".F == "1/-2".F) -> false,
    ("1/2".F == "-1/2".F) -> false,
    ("1/2".F == "1/2".F) -> true,
    ("-1/2".F > "1/-2".F) -> false,
    ("-1/2".F > "-1/2".F) -> false,
    ("-1/2".F > "1/2".F) -> false,
    ("1/2".F > "1/-2".F) -> true,
    ("1/2".F > "-1/2".F) -> true,
    ("1/2".F > "1/2".F) -> false,
    ("-1/2".F >= "1/-2".F) -> true,
    ("-1/2".F >= "-1/2".F) -> true,
    ("-1/2".F >= "1/2".F) -> false,
    ("1/2".F >= "1/-2".F) -> true,
    ("1/2".F >= "-1/2".F) -> true,
    ("1/2".F >= "1/2".F) -> true
  )
  val Fraction_compute_checkList = List(
    ((2 + 3.F).getClass.getName) -> "sfraction.Fraction",
    ((2 - 3.F).getClass.getName) -> "sfraction.Fraction",
    ((2 * 3.F).getClass.getName) -> "sfraction.Fraction",
    ((2 / 3.F).getClass.getName) -> "sfraction.Fraction",
    ((3 === 3.F)) -> true,
    ((2 + 3.F) == 5.F) -> true,
    ((2 - 3.F) == (-1.F)) -> true,
    ((2 * 3.F) == 6.F) -> true,
    ((2 / 3.F) == "2/3".F) -> true
  )
  val Fraction_mathOps_checkList = List(
    ((1.F + 1.F).toString) -> "2",
    (("1/2".F + "1/4".F).toString) -> "(3/4)",
    (("1/2".F - "1/4".F).toString) -> "(1/4)",
    (("1/2".F * "1/4".F).toString) -> "(1/8)",
    (("1/2".F / "1/4".F).toString) -> "2",
    (("1/2".F.abs).toString) -> "(1/2)",
    (("1/2".F.negate).toString) -> "(-1/2)",
    (("1/2".F.inv).toString) -> "2", // 假设实现了求倒数
    (("1/2".F.min("3/4".F)).toString) -> "(1/2)",
    (("1/2".F.max("3/4".F)).toString) -> "(3/4)"
  )
  val Matrix_ops_checkList = List(
    Matrix((-1.F / 2, 2), (3, 4), (5, 6)) -> Matrix((-1.F / 2, 2), (3, 4), (5, 6)),
    (Matrix((-1.F / 2, 2), (3, 4), (5, 6)).T) ->
      Matrix(((-1.F / 2), 3, 5), (2, 4, 6)),
    ((Matrix((-1.F / 2, 2), (3, 4), (5, 6)) + Matrix((1, 0), (0, 1), (0, 0)))) ->
      Matrix(((1.F / 2), 2), (3, 5), (5, 6)),
    ((Matrix((-1.F / 2, 2), (3, 4), (5, 6)) * 2.F)) ->
      Matrix((-1.F, 4), (6, 8), (10, 12)),
    ((Matrix((-1.F / 2, 2), (3, 4)) * Matrix((1, 0), (0, 1)))) ->
      Matrix((-1.F / 2, 2), (3, 4)) // 确保矩阵乘法逻辑正确
  )

  println("====start====")

  check(Matrix_ops_checkList)

  val m = Matrix((-1.F / 2, 2), (3, 4), (5, 6))
  val n = Matrix(
    (1, 2, 3, 1),
    (2, -1, 1, -3),
    (1, 0, 1, -1),
    (10, 2, 5, 1),
    (1, 2, 6, 1),
    (1, 6, 3, 3)
  )
  val p = Matrix((-1.F / 2, 2), (3, 4)).toSquareMatrix

  val c   = List(1, 0, 1, 0, 1)
  var c_e = c ++ List(c.reduce(_ ^ _))
  c_e = c_e.updated(1, 1)
  val c_d = c_e.reduce(_ ^ _)
  println(c_e)
  println(c_d)
  print(n.mod(2))

  val v = Vec(1, 3, 2)
  println((v, v, v).Mat.toPartColVecMatrix)
  // println("\n====ECC start====")
  // test_ECC()

  test_660()
}
