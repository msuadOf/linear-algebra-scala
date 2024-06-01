package linearAlgebra

import sfraction._
import sfraction.HasFraction._
// import scala.math.Numeric.Implicits._

import linearAlgebra.Implicits._

object utils {
  def E(n: Int): SquareMatrix = Matrix.E(n)
  def E(n: Int, i: Int, j: Int): SquareMatrix = Matrix.E(n, i, j)
  def E(n: Int, i: Int, j: Int, k: Fraction): SquareMatrix = Matrix.E(n, i, j, k)
  def E(n: Int, i: Int, k: Fraction): SquareMatrix = Matrix.E(n, i, k)
  def E(n: Int, k: Fraction): SquareMatrix = Matrix.E(n, k)

  def VecToMatrix(vecs:           Array[Vec]): Matrix           = PartColVecMatrix(vecs).toMatrix
  def VecToPartColVecMatrix(vecs: Array[Vec]): PartColVecMatrix = PartColVecMatrix(vecs)
  def VecToMatrix(vecs:           Vec*):       Matrix           = VecToMatrix(vecs.toArray)
}
