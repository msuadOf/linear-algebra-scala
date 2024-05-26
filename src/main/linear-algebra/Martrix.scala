// package linearAlgebra

// import sfraction._
// import sfraction.HasFraction._

// class DenseMatrix[T<:Fraction] private (val rows: Array[Array[T]]) {
//   // 修复构造函数中的错误，确保元组转换为数组
//   def this(rows: (T, T)*) = {
//     this(rows.map(row => row.productIterator.toArray).toArray)
//   }

//   def numRows: Int = rows.length
//   def numCols: Int = rows.headOption.getOrElse(Array.empty).length

//   // 修复apply方法，使其为泛型方法
//   def apply(i: Int, j: Int): T = rows(i)(j)

//   // 修复update方法，使其为泛型方法
//   def update(i: Int, j: Int, value: T): Unit = {
//     rows(i)(j) = value
//   }

//   // 打印矩阵
//   override def toString: String = rows.map(_.mkString("(", ", ", ")")).mkString("\n")

//   // 可以根据需要添加更多矩阵操作的方法，例如加法、乘法等
// }

// object DenseMatrix {
//   // 修复apply方法，使其为泛型方法
  
// def apply[T](rows: (T, T)*): DenseMatrix[T] = new DenseMatrix(rows: _*)

// }