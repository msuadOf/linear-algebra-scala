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

}
