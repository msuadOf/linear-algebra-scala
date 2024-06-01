package linearAlgebra

import sfraction._
import sfraction.HasFraction._

//Parallel
import scala.collection.parallel.CollectionConverters._
abstract class MatrixLike(val rows: Array[Array[Fraction]]) {
  def numRows:  Int     = rows.length
  def numCols:  Int     = rows.headOption.getOrElse(Array.empty).length
  def isSquare: Boolean = rows.length > 0 && rows.forall(_.length == rows.length)

  def toMatrix:       Matrix       = new Matrix(rows)
  def toSquareMatrix: SquareMatrix = new SquareMatrix(rows)
  def toS = toSquareMatrix
  def S   = toSquareMatrix
  def toAugmentedMatrix(DivideColNumber: Int): AugmentedMatrix = new AugmentedMatrix(this.rows, DivideColNumber)
}

class Matrix(override val rows: Array[Array[Fraction]]) extends MatrixLike(rows) {

  def this(matrix: Matrix) = this(matrix.rows.clone().map(_.clone()))

  def copy() = new Matrix(this)
  // def this(rows: Seq[Product]) = {
  //   this(rows.map(row => row.productIterator.toArray).toArray)
  // }

  // 修复apply方法，使其为泛型方法
  def apply(i: Int, j: Int): Fraction = rows(i)(j)
  def apply(i: Int) = rows(i)

  def rowSelected(i: Int) = rows(i)
  def colSelected(j: Int) = rows.map(_(j))

  // 修复update方法，使其为泛型方法
  def update(i: Int, j: Int, value: Fraction): Matrix = {
    val a = rows
    a(i)(j) = value
    Matrix(a)
  }
  def updateRow(rowIndex: Int, newRow: Array[Fraction]): Matrix = {
    val updatedRows = rows.take(rowIndex) ++ Array(newRow) ++ rows.drop(rowIndex + 1)
    new Matrix(updatedRows)
  }
  def removeRow(rowIndexs: Seq[Int]): Matrix = {
    val updatedRows = rows.zipWithIndex.filterNot { case (row, idx) => !rowIndexs.forall(idx != _) }
    new Matrix(updatedRows.map(_._1))
  }
  def removeCol(colIndexs: List[Int]): Matrix = {
    this.T.removeRow(colIndexs).T
  }
  def removeCol(colIndexs: Int*): Matrix = removeCol(colIndexs.toList)
  // 打印矩阵右对齐
  var toStringAlignNum: Int = 1
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
  def toString(TypeTag: String): String = {
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
  override def toString: String = toString("Matrix")
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
      "The number of columns(" + numCols + ") in the first matrix must equal the number of rows(" + that.numRows + ") in the second matrix for multiplication."
    )

    val resultRows = for (i <- 0 until numRows) yield {
      for (j <- 0 until that.numCols) yield {
        (0 until numCols).map(k => rows(i)(k) * that.rows(k)(j)).foldLeft(Fraction(0, 1))(_ + _)
      }
    }
    new Matrix(resultRows.map(_.toArray).toArray)
  }
  def *^(that: Matrix): Matrix = {
    require(
      numCols == that.numRows,
      "The number of columns(" + numCols + ") in the first matrix must equal the number of rows(" + that.numRows + ") in the second matrix for multiplication."
    )
    rows.map(_.map(n => require(n == 1.F || n == 0.F, "The matrix must be binary matrix.")))
    val resultRows = for (i <- 0 until numRows) yield {
      for (j <- 0 until that.numCols) yield {
        (0 until numCols).map(k => rows(i)(k).v._1 ^ that.rows(k)(j).v._1).foldLeft(Fraction(0, 1)) { (a, c) =>
          (a.v._1 & c.v._1).F
        }
      }
    }
    new Matrix(resultRows.map(_.toArray).toArray)
  }
  def unary_- : Matrix = {
    new Matrix(rows.map(_.map(-_)))
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

  def swapRow(i: Int, j: Int): Matrix = this.updateRow(j, rows(i)).updateRow(i, rows(j))
  def swapCol(i: Int, j: Int): Matrix = this * Matrix.E(this.numCols, i, j)
  //=============================

  def xiaoquyuansu(Origin: (Int, Int), Target: (Int, Int)) = {
    val (i, j) = (Origin._1, Target._1)
    val bili   = rows(Target._1)(Target._2).F / rows(Origin._1)(Origin._2).F
    (Matrix.E(this.numRows, i, j, -bili) * this).rows(Target._1)
  }
  def xiaoquxiamianyuansu(i: Int, j: Int) = {
    Matrix(rows.zipWithIndex.map {
      case (row, index) =>
        if (index <= i) row
        else {
          val t = xiaoquyuansu((i, j), (index, j))
          if (t(j + 1) == 0.F) t
          else t.map(_ / t(j + 1))
        }
    })
  }
  // 转换为行阶梯形
  def toRowEchelonForm: Matrix = {
    // 使用foldLeft遍历每一行，逐步构建新的矩阵
    //(0 until numRows).foldLeft(this) { (currentMatrix, row) =>
    var currentMatrix = this
    var row           = 0
    while (row < numRows) {
      // 已经处理过的列，避免重复操作
      val processedCols = (0 until row).toSet
      // 在当前行寻找第一个非零元素的列索引，跳过已处理列
      val pivotCol = (row until numCols).find(c => !currentMatrix(row, c).isZero && !processedCols.contains(c))

      pivotCol match {
        case Some(pivot) =>
          // 保证主元为1
          val scale     = currentMatrix(row, pivot).inverse
          val scaledRow = currentMatrix.rows(row).map(_ * scale)
          val newRow = scaledRow.zipWithIndex.map {
            case (v, i) if i == pivot => v
            case (v, _)               => v
          }

          // 更新当前行，并对后续行进行消元操作
          val updatedMatrix = currentMatrix.updateRow(row, newRow)
          //并行
          val clearedBelow = (row + 1 until numRows).par.foldLeft(updatedMatrix) { (mat, r) =>
            val factor = mat(r, pivot)
            if (!factor.isZero) {
              // 用当前行消去下一行的相应元素
              val newRowBelow = mat.rows(r).zip(scaledRow).map {
                case (v, pv) => v - (pv * factor)
              }
              mat.updateRow(r, newRowBelow)
            } else mat // 如果系数为0，不需要消去，直接保留当前矩阵
          }
          currentMatrix = clearedBelow
          //branch 1
          row = row + 1
        case None =>
          // 当前行全为0，直接跳过，移到最后一行
          def moveRowToLastRow(matrix: Matrix, rowIndex: Int) =
            (rowIndex until matrix.numRows - 1).foldLeft(matrix)((accMatrix, rowIndex) =>
              accMatrix.swapRow(rowIndex, rowIndex + 1)
            )
          currentMatrix = moveRowToLastRow(currentMatrix, row)
          //branch 2 重新开始本行的计算
          if ((row until numRows).forall(r => { (row until numCols).forall(c => currentMatrix(r, c).isZero) }))
            row    = numRows //ends
          else row = row
      }

    }
    currentMatrix
  }

  // 转换为行最简形
  def toReducedRowEchelonForm: Matrix = {
    // 首先获取行阶梯形矩阵
    val rowEchelonMatrix = toRowEchelonForm

    // 从最后一行开始，处理每一行
    (0 until numRows).foldRight(rowEchelonMatrix) { (row, currentMatrix) =>
      //[ ] 如果该行全为0，跳过
      // 已经处理过的列，避免重复操作
      val processedCols = (0 until row).toSet
      // 在当前行寻找第一个非零元素的列索引，跳过已处理列
      val pivotCol = (row until numCols).find(c => !currentMatrix(row, c).isZero && !processedCols.contains(c))

      pivotCol match {
        case Some(pivot) =>
          //并行
          val clearedAbove = (0 until row).par.foldRight(currentMatrix) { (r, mat) =>
            val factor    = mat(r, pivot)
            val scaledRow = currentMatrix.rows(row)
            if (!factor.isZero) {
              // 用当前行消去下一行的相应元素
              val newRowBelow = mat.rows(r).zip(scaledRow).map {
                case (v, pv) => v - (pv * factor)
              }
              mat.updateRow(r, newRowBelow)
            } else mat // 如果系数为0，不需要消去，直接保留当前矩阵
          }
          clearedAbove
        case None =>
          // 当前行全为0，直接跳过
          currentMatrix
      }
    }

  }

  //秩
  def rank: Int = {
    val rref = toRowEchelonForm
    numRows - (0 until rref.numRows).count(r => (0 until rref.numCols).forall(c => rref(r, c).isZero))
  }
  def r: Int = rank
}

object Matrix {

  // def apply(rows: Product*): Matrix = new Matrix(rows)
  def apply(rows: Array[Fraction]*):       Matrix = new Matrix(rows.toArray)
  def apply(rows: Array[Array[Fraction]]): Matrix = new Matrix(rows)
  def Identity(size: Int): SquareMatrix = {
    val identityRows = (0 until size).map(i =>
      Array
        .tabulate(size)(_ == i)
        .map(i =>
          Fraction(i match {
            case true  => 1
            case false => 0
          })
        )
    )
    new SquareMatrix(identityRows.toArray)
  }
  def E(n: Int): SquareMatrix = Identity(n)
  def E(n: Int, i: Int, j: Int): SquareMatrix = E(n).swapRow(i, j).toSquareMatrix
  def E(n: Int, i: Int, j: Int, k: Fraction): SquareMatrix = E(n).update(j, i, k).toSquareMatrix
  def E(n: Int, i: Int, k: Fraction): SquareMatrix = E(n).update(i, i, k).toSquareMatrix
  def E(n: Int, k: Fraction): SquareMatrix = E(n).scalarMultiply(k).toSquareMatrix
}
