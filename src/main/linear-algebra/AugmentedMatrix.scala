package linearAlgebra

import sfraction._
import sfraction.HasFraction._

class AugmentedMatrix(val m: Matrix, val n: Matrix) extends Matrix(m.rows.zip(n.rows).map(r => r._1 ++ r._2)) {
  require(m.numRows == n.numRows, "[AugmentedMatrix]:Matrix(numRows " + m.numRows + "," + n.numRows + ") size mismatch")
  val DivideColNumber = m.numRows

  def this(rows: Array[Array[Fraction]], DivideColNumber: Int) = {
    this(Matrix(rows.map(r => r.take(DivideColNumber))), Matrix(rows.map(r => r.drop(DivideColNumber))))
  }

  def setDivideColNumber(colIndex: Int): AugmentedMatrix = {
    new AugmentedMatrix(this.rows, colIndex)
  }
  def getDivideColNumber() = DivideColNumber

  // m.rows.zip(n.rows).map(r => r._1 ++ r._2)
  def apply(): (Matrix, Matrix) = {
    (m, n)
  }
  override def toReducedRowEchelonForm: AugmentedMatrix = {
    super.toReducedRowEchelonForm.toAugmentedMatrix(this.getDivideColNumber())
  }
  override def toString(TypeTag: String): String = {
    // 计算每列最大宽度，包括可能的负号和斜杠（针对Fraction的表示）
    val maxOverallWidth = rows.flatMap(_.map(_.toString.length)).max
    val columnWidths    = rows.head.indices.map(i => rows.map(_(i).toString.length).max)

    // 格式化并打印矩阵，确保整体右对齐
    "\n" + TypeTag + ("[" + numRows + "x" + numCols + "]") + ":\n" + rows.map { row =>
      row
        .zip(columnWidths)
        .map {
          case (elem, width) =>
            // 为每个元素添加前置空格以确保从矩阵左边界开始的整体右对齐
            val totalPadding = maxOverallWidth - elem.toString.length
            toStringAlignNum match {
              case 1  => " " * (totalPadding) + elem.toString
              case 0  => " " * (totalPadding / 2) + elem.toString + " " * ((totalPadding + 1) / 2)
              case -1 => elem.toString + " " * (totalPadding)
            }
        }
        .mkString("[", ", ", "]")
    }.mkString("\n")
  }

}
object AugmentedMatrix {
  def apply(m:    Matrix, n:                               Matrix): AugmentedMatrix = new AugmentedMatrix(m, n)
  def apply(rows: Array[Array[Fraction]], DivideColNumber: Int): AugmentedMatrix =
    new AugmentedMatrix(rows, DivideColNumber)
}
