package linearAlgebra

import sfraction._
import sfraction.HasFraction._

class PartMatrix(rows: Array[Array[Fraction]]) extends Matrix(rows) {
  override def toString: String = toString("PartMatrix")
}

class PartColVecMatrix(rows: Array[Array[Fraction]]) extends PartMatrix(rows) {
  def this(vecs: Array[Vec]) = {
    this((new Matrix(vecs.map(_.T.rows(0)).toArray)).T.rows)
  }
  override def toString: String = {
    // 计算每列最大宽度，包括可能的负号和斜杠（针对Fraction的表示）
    val maxOverallWidth = rows.flatMap(_.map(_.toString.length)).max
    val columnWidths    = rows.head.indices.map(i => rows.map(_(i).toString.length).max)

    // 格式化并打印矩阵，确保整体右对齐
    "\n" + "PartColVecMatrix" + ("[" + numRows + "x" + numCols + "]") + ":\n" + rows.map { row =>
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
        .mkString("[", " | ", "]")
    }.mkString("\n")
  }

}
object PartColVecMatrix {
  def apply(vecs: Array[Vec]): PartColVecMatrix = new PartColVecMatrix(vecs)
}
