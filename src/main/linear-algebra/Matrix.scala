package linearAlgebra

import sfraction._
import sfraction.HasFraction._
case class Matrix private (val rows: Array[Array[Fraction]]) {

  // def this(rows: Seq[Product]) = {
  //   this(rows.map(row => row.productIterator.toArray).toArray)
  // }

  def numRows: Int = rows.length
  def numCols: Int = rows.headOption.getOrElse(Array.empty).length

  // 修复apply方法，使其为泛型方法
  def apply(i: Int, j: Int): Fraction = rows(i)(j)
  def apply(i: Int) = rows(i)

  // 修复update方法，使其为泛型方法
  def update(i: Int, j: Int, value: Fraction): Unit = {
    rows(i)(j) = value
  }

  // 打印矩阵右对齐
  private var toStringAlignNum: Int = 1
  def toStringSetAlignStyle(info: String) = {
    toStringAlignNum = info match {
      case "right"  => 1
      case "center" => 0
      case "left"   => -1
    }
    this
  }
  def toStringSetAlignStyle(info: Int) = {
    toStringAlignNum = info match {
      case positive if positive > 0 => 1
      case 0                        => 0
      case negative if negative < 0 => -1
    }
    this
  }
  /* override def toString: String = {

  // 计算每列最大宽度
  val columnWidths = rows.flatMap(_.map(_.toString.length)).foldLeft(Array.fill(rows.head.length)(0)) { (maxWidths, len) =>
    maxWidths.zipWithIndex.map { case (width, i) => math.max(width, len) }
  }

  // 格式化并打印矩阵，直接内联右对齐逻辑
  rows.map { row =>
    row.zip(columnWidths).map { case (elem, width) =>
      // 内联右对齐逻辑
      val elemStr = elem.toString
      if (elemStr.length >= width) elemStr
      else {
        val padding = " " * (width - elemStr.length)
                        toStringAlignNum match {
          case 1 => padding+elemStr
          case 0 => " " * ((width - elemStr.length) / 2) + elem.toString + " " * (((width - elemStr.length) + 1) / 2)
          case -1 => elemStr+padding
         }
      }

    }.mkString("[", ", ", "]")
  }.mkString("\n")
} */
  override def toString: String = {
    // 计算每列最大宽度，包括可能的负号和斜杠（针对Fraction的表示）
    val maxOverallWidth = rows.flatMap(_.map(_.toString.length)).max
    val columnWidths    = rows.head.indices.map(i => rows.map(_(i).toString.length).max)

    // 格式化并打印矩阵，确保整体右对齐
    "\nMatrix:\n" + rows.map { row =>
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
//   // 可以根据需要添加更多矩阵操作的方法，例如加法、乘法等
  /**
    * 转置矩阵
    */
  def T: Matrix = {
    val transposedRows = (0 until numCols).map(i => rows.map(_.apply(i)))
    new Matrix(transposedRows.toArray)
  }

  /**
    * 矩阵相加
    */
  def +(that: Matrix): Matrix = {
    require(numRows == that.numRows && numCols == that.numCols, "Matrices must have the same dimensions for addition.")
    val resultRows = rows.zip(that.rows).map {
      case (row1, row2) =>
        row1.zip(row2).map { case (elem1, elem2) => elem1 + elem2 }
    }
    new Matrix(resultRows.toArray)
  }

  /**
    * 数乘矩阵
    */
  def scalarMultiply(scalar: Fraction): Matrix = {
    val resultRows = rows.map(_.map(_ * scalar))
    new Matrix(resultRows.toArray)
  }
  def *(that: Fraction): Matrix = this.scalarMultiply(that)

  /**
    * 矩阵相乘
    */
  def *(that: Matrix): Matrix = {
    require(
      numCols == that.numRows,
      "The number of columns in the first matrix must equal the number of rows in the second matrix for multiplication."
    )

    val resultRows = for (i <- 0 until numRows) yield {
      for (j <- 0 until that.numCols) yield {
        (0 until numCols).map(k => rows(i)(k) * that.rows(k)(j)).foldLeft(Fraction(0, 1))(_ + _)
      }
    }
    new Matrix(resultRows.map(_.toArray).toArray)
  }
  def equal(that: Matrix): Boolean = {
    val a = this.rows
    val b = that.rows
    a.length == b.length && a.zip(b).forall {
      case (rowA, rowB) =>
        rowA.length == rowB.length && rowA.zip(rowB).forall { case (elementA, elementB) => elementA == elementB }
    }
  }
  def ==(that:  Matrix): Boolean = equal(that)
  def ===(that: Matrix): Boolean = equal(that)
}

object Matrix {

  // def apply(rows: Product*): Matrix = new Matrix(rows)
  def apply(rows: Array[Fraction]*): Matrix = new Matrix(rows.toArray)
}
