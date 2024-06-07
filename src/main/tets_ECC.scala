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
  (1 until (12)).foreach(i => {
    print(s"==${i}:")
    w = w.update(12 - i, 0, (w(12 - i)(0) + 1).v._1 % 2)
    val x             = (w.T * H.T).mod(2)
    val index_changed = x.T.toVec.asBinary.v._1
    w = w.update(11 - index_changed, 0, (w(11 - index_changed)(0) + 1).v._1 % 2)
    println(x.T.toVec.asBinary)
    println(x)
  })
  val vec = Vec(1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1)
  println(vec.asBinary)
}
case class ECC_72_64() {
  def isSomeBitField(v: Int, pos: Int) = (v & (1 << pos)) != 0
  def powOfTwo(v:       Int) = 1 << v
  val r       = 4 //7
  val m       = 8 //64
  val arr_gen = (j: Int) => (1 until (m + r) + 1).map(i => if (isSomeBitField(i, j)) (1.F) else (0.F)).toArray
  val arr     = (0 until r).map(i => arr_gen(i))
  val rmList  = (0 until (r)).map(i => (1 << i) - 1)
  println((0 until 10).map(i => powOfTwo(i)))
  println(isSomeBitField(1, 1))
  println(Matrix(arr.toArray).removeCol(rmList.toArray) === ECC_12_8().Q)
  println(rmList)
  /*     val Q = Matrix(
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
object ECC_App {
  def apply(err_pos: Int)= {
    val m = 64
    val r = 7
    def isSomeBitField(v: Int, pos: Int) = (v & (1 << pos)) != 0
    def powOfTwo(v:       Int) = 1 << v
    object ECC_MartixQH_gen {
      def apply(m: Int, r: Int): (Matrix, Matrix) = {
        val arr_gen = (j: Int) => (1 until (m + r) + 1).map(i => if (isSomeBitField(i, j)) (1.F) else (0.F)).toArray
        val arr     = (0 until r).map(i => arr_gen(i))
        val rmList  = (0 until (r)).map(i => (1 << i) - 1)
        println(arr.toArray.head.length)
        val Q = Matrix(arr.toArray).removeCol(rmList.toArray)
        val H = Matrix(arr.toArray)
        (Q, H)
      }
    }

    val t = ECC_MartixQH_gen(m, r)
    val Q = t._1
    val H = t._2
    println(Q.numCols)
    val P = Q.T

    var i_d = 0
    var i_p = 0
    val G = (0 until (m + r))
      .map(i => {
        if (i == ((1 << i_p) - 1)) {
          val a = Q.apply(i_p)
          i_p = i_p + 1
          a
        } else {
          val a = Array.tabulate(Q.numCols)(i => if (i == i_d) 1.F else 0.F)
          i_d = i_d + 1
          a
        }
      })
      .toArray
      .Mat

    val s = Matrix(
      (1 to m).map(_ => Random.nextInt(2)).toArray.map(_.F)
    ).T.toVec

    var w = (G * s).mod(2)
    println("[s]" + s)
    println("[G]" + G)
    println("[G*s]" + w.toVec)
    println("[H]" + H)
    println("[H*w]" + (H *^ w).toVec)

    val x = (H * w.toVec).mod(2)

    val pos = err_pos
    println("[w]" + w.toVec)
    w = w.update(pos - 1, 0, (w(pos - 1)(0) + 1).v._1 % 2)

    println("[w]" + w.toVec)
    println(s"===${err_pos}===[H*w]" + (H * w).mod(2).toVec.reverse.asBinary)
    err_pos.F==((H * w).mod(2).toVec.reverse.asBinary)
  }
}
case class test_ECC() {
  println( (1 until 72).map(i => ECC_App(i)).forall(i=>i) )
}
