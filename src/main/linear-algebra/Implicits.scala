package linearAlgebra
import sfraction._
import sfraction.HasFraction._
object Implicits {

  import scala.language.implicitConversions
  implicit def TuppleToArray(t: Product): Array[Fraction] = t.productIterator.toArray.map {
    case num: Int      => num.F
    case num: String   => num.F
    case num: Fraction => num
    case x => throw new IllegalArgumentException(s"[Fraction]Unsupported type: ${x.getClass.getName}")
  }
  // 使用隐式转换来支持操作符重载
  //一阶方阵
  implicit def fractionToOneStepMatrix(f: Fraction): Matrix = Matrix(Array(Array(f)))

  // 重载操作符 *
  implicit class fractionMatrixExtension(private val f: Fraction) {
    def *(that: Any): Matrix = that match {
      case that: Fraction => f * that
      case that: Matrix   => that.scalarMultiply(f)
      case _ => throw new IllegalArgumentException("Unsupported operation for type.")
    }
  }
  implicit class intMatrixExtension(private val f: Int) {
    def *(that: Matrix): Matrix = that.scalarMultiply(Fraction(f))
    //[ ]TODO: 重载操作符 +
    // def +(that: Matrix): Matrix = that.scalarAdd(Fraction(f))
  }
  implicit class TuppleMatrixExtension(private val t: (Matrix, Matrix)) {
    def A: AugmentedMatrix = AugmentedMatrix(t._1, t._2)
  }

  implicit class MatrixExtension(private val m: Matrix) {
    implicit class FractionModExtension(private val f: Fraction) {
      def %(n: Int): Fraction = {
        require(f.v._2 == 1, "[Implicit::Matrix::FractionModExtension]deno should be 1 ,while it is " + f.v._2)
        f.v._1 % n
      }
    }
    def mod(mod_n: Int): Matrix = Matrix(m.rows.map(r => r.map(_ % mod_n)))
  }

}
