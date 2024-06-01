package linearAlgebra

import sfraction._
import sfraction.HasFraction._

class Vec(row: Array[Fraction]) extends Matrix(Matrix(row).T) {
  def this(rows: Array[Array[Fraction]]) = {
    this(Matrix(rows).colSelected(0))
    require(rows.headOption.getOrElse(Array.empty).length == 1, "[Vec] must be a column matrix.")
  }
  def this(mat: Matrix) = {
    this(mat.rows)
  }

  override def toString = s"\nVec[${numRows}]\n(${row.mkString(", ")})"
}
object Vec {
  def apply(row: Array[Fraction]): Vec = new Vec(row)
  def apply(row: Fraction*):       Vec = new Vec(row.toArray)
}
class ColVec(row: Array[Fraction]) extends Vec(row) {
  override def toString = s"\nColVec[${numRows}]\n(${row.mkString(", ")})"

}
// RowVec是错的
class RowVec(row: Array[Fraction]) extends Vec(Vec(row).rows.head) {
  override def toString = s"\nRowVec[${numRows}]\n(${row.mkString(", ")})"
}
