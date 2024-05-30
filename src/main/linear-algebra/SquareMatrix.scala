package linearAlgebra
import sfraction._
import sfraction.HasFraction._

import linearAlgebra.Implicits._
class SquareMatrix(override val rows: Array[Array[Fraction]]) extends Matrix(rows) {
  require(
    rows.length > 0 && rows.forall(_.length == rows.length),
    "[SquareMatrix]:[" + numRows + "x" + numCols + "], not a square matrix." + this.toString
  )
  def n = numRows
  override def toString: String = toString("SquareMatrix")

  // 计算行列式
  def det: Fraction = determinant()
  def determinant(): Fraction = {
    require(numRows == numCols, "Matrix must be square to calculate determinant.")

    if (numRows == 1) rows(0)(0) // 1x1矩阵的行列式是其唯一元素
    else if (numRows == 2) { // 2x2矩阵的行列式计算
      (rows(0)(0) * rows(1)(1) - rows(0)(1) * rows(1)(0))
    } else {
      var det = Fraction(0, 1)
      for (j <- 0 until numRows) {
        // 计算代数余子式，注意行列式计算中的正负号交替
        val sign     = if ((j % 2) == 0) Fraction(1, 1) else Fraction(-1, 1)
        val cofactor = removeRowAndColumn(0, j) // 移除第一行与当前列
        det += sign * rows(0)(j) * cofactor.determinant()
      }
      det
    }
  }

  private def removeRowAndColumn(removeRowIndex: Int, removeColIndex: Int): SquareMatrix = {
    val filteredRows = rows.zipWithIndex.filter { case (_, index) => index != removeRowIndex }.map {
      case (row, _) => row.zipWithIndex.filter { case (_, colIndex) => colIndex != removeColIndex }.unzip._1
    }
    SquareMatrix(filteredRows.toArray)
  }
  def inverse: SquareMatrix = {
    require(r == n , "Matrix must be square to calculate inverse.")
    val (_:Matrix,inversed:Matrix)=AugmentedMatrix(this,Matrix.E(n)).toReducedRowEchelonForm ()
    inversed.toSquareMatrix
  }
  override def *(that: Fraction): SquareMatrix = (super.*(that) ).toSquareMatrix
  def adjoint: SquareMatrix={
    inverse * (det)
  }
}

// 提供工厂方法或转换方法
object SquareMatrix {
  def apply(rows: Array[Array[Fraction]]): SquareMatrix = new SquareMatrix(rows)

}
