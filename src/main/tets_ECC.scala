import sfraction._
import sfraction.HasFraction._
import scala.math.Numeric.Implicits._

import linearAlgebra._
import linearAlgebra.utils._
import linearAlgebra.Implicits._

import scala.util.Random

case class ECC_12_8() {
  val Q = Matrix(
    (1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    (0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0),
    (0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
  ).removeCol(1 - 1, 2 - 1, 4 - 1, 8 - 1)
  val P = Q.T
  val G = (E(P.numRows), P).A
  val H = (P.T, E(P.T.numRows)).A
  val s = Matrix(
    (1, 0, 0, 1, 1, 1, 0, 0)
  ).T
  var w = ((s.T * G).mod(2)).T

  println((Q.T).mod(2))
  println((s.T * G).mod(2))
  println((w.T * H.T).mod(2))
  println(w.T)
  w = w.update(11, 0, (w(11)(0) + 1).v._1 % 2)
  println((w.T * H.T).mod(2))
  println(w.T)
}
case class ECC_72_64() {
  /*   val Q = Matrix(
    (1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    (0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0),
    (0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
  ).removeCol(1 - 1, 2 - 1, 4 - 1, 8 - 1, 16 - 1, 32 - 1, 64 - 1)
  val P = Q.T
  val G = (E(P.numRows), P).A
  val H = (P.T, E(P.T.numRows)).A
  val s = Matrix(
    (1 to 64).map(_ => Random.nextInt(2)).toArray.map(_.F)
  ).T
  var w = ((s.T * G).mod(2)).T

  println((Q.T).mod(2))
  println((s.T * G).mod(2))
  println((w.T * H.T).mod(2))
  println(w.T)
  w = w.update(11, 0, (w(11)(0) + 1).v._1 % 2)
  println((w.T * H.T).mod(2))
  println(w.T) */
}
case class test_ECC() {
  ECC_72_64()
}
