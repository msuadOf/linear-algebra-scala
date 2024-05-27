package sfraction
import scala.math.BigInt

/* case class Fraction(val real: Double, val imaginary: Double) extends Numeric[Fraction] {
  def plus(x: Fraction, y: Fraction): Fraction = new Fraction(x.real + y.real, x.imaginary + y.imaginary)
  def minus(x: Fraction, y: Fraction): Fraction = new Fraction(x.real - y.real, x.imaginary - y.imaginary)
  def times(x: Fraction, y: Fraction): Fraction = new Fraction(x.real * y.real - x.imaginary * y.imaginary, x.real * y.imaginary + x.imaginary * y.real)
  def negate(x: Fraction): Fraction = new Fraction(-x.real, -x.imaginary)

  def fromInt(x: Int): Fraction = new Fraction(x.toDouble, 0.0)
  def toInt(x: Fraction): Int = x.real.toInt
  def toLong(x: Fraction): Long = x.real.toLong
  def toFloat(x: Fraction): Float = x.real.toFloat
  def toDouble(x: Fraction): Double = x.real

  def parseString(str: String): Option[Fraction] = {
    val parts = str.strip().split("\\s+")
    if (parts.length != 2) None
    else {
      try {
        val real = parts(0).toDouble
        val imaginary = parts(1).toDouble
        Some(new Fraction(real, imaginary))
      } catch {
        case _: NumberFormatException => None
      }
    }
  }

  def compare(x: Fraction, y: Fraction): Int = x.real.compareTo(y.real)
} */

case class Fraction private (val v: (Int, Int)) extends Numeric[Fraction] {
  def numerator   = v._1
  def denominator = v._2
  def fenzi       = numerator
  def fenmu       = denominator
  def simplify(): Fraction = {
    require(
      denominator != 0,
      "package sfraction::Fraction-simplify::The Denominator of a Fraction should not be equal."
    )
    val gcd: Int = (BigInt(numerator).gcd(BigInt(denominator))).toInt
    val v_g        = (numerator / gcd, denominator / gcd)
    val v_g_signed = (v_g._1 * v_g._2.sign, v_g._2.abs)
    Fraction(v_g_signed)
  }
  def s = simplify()
  override def toString(): String = {
    if (fenmu != 1) "(" + fenzi.toString() + "/" + fenmu.toString() + ")"
    else fenzi.toString()
  }
  def plus(x:   Fraction, y: Fraction): Fraction = x.do_+(y)
  def minus(x:  Fraction, y: Fraction): Fraction = x.do_-(y)
  def times(x:  Fraction, y: Fraction): Fraction = x.do_*(y)
  def negate(x: Fraction): Fraction = Fraction(0).do_-(x)
  def negate: Fraction = Fraction(0).do_-(this)

  def fromInt(x:  Int):      Fraction = Fraction(x)
  def toInt(x:    Fraction): Int      = x.fenzi / x.fenmu
  def toLong(x:   Fraction): Long     = x.fenzi.toLong / x.fenmu.toLong
  def toFloat(x:  Fraction): Float    = x.fenzi.toFloat / x.fenmu.toFloat
  def toDouble(x: Fraction): Double   = x.fenzi.toDouble / x.fenmu.toDouble
  def parseString(str: String): Option[Fraction] = {
    None
    /*    val parts = str.strip().split("\\s+")
    if (parts.length != 2) None
    else {
      try {
        val real = parts(0).toInt
        val imaginary = parts(1).toInt
        Some(Fraction(real, imaginary))
      } catch {
        case _: NumberFormatException => None
      }
    } */
  }
  def compare(x: Fraction, y: Fraction): Int = (x.do_-(y)).simplify().fenzi.sign

  def +(that:   Fraction): Fraction = do_+(that)
  def -(that:   Fraction): Fraction = do_-(that)
  def *(that:   Fraction): Fraction = do_*(that)
  def /(that:   Fraction): Fraction = do_/(that)
  def x(that:   Fraction): Fraction = *(that)
  def >(that:   Fraction): Boolean  = gt(this, that)
  def >=(that:  Fraction): Boolean  = gteq(this, that)
  def <(that:   Fraction): Boolean  = lt(this, that)
  def <=(that:  Fraction): Boolean  = lteq(this, that)
  def ===(that: String):   Boolean  = do_===(that)

  def abs: Fraction = Fraction(Math.abs(numerator), Math.abs(denominator))
  def inv: Fraction = Fraction(denominator, numerator)
  def min(that: Fraction): Fraction = if (this < that) this else that
  def max(that: Fraction): Fraction = if (this > that) this else that

  def do_+(that: Fraction): Fraction = {
    val that_s = that.simplify()
    val this_s = this.simplify()
    Fraction(
      this_s.numerator * that_s.denominator + that_s.numerator * this_s.denominator,
      this_s.denominator * that_s.denominator
    )
  }
  def do_*(that: Fraction): Fraction = {
    val that_s = that.simplify()
    val this_s = this.simplify()
    Fraction(this_s.fenzi * that_s.fenzi, this_s.fenmu * that_s.fenmu)
  }
  def do_-(that: Fraction): Fraction = {
    val that_s = that.simplify()
    val this_s = this.simplify()
    this_s + that_s * Fraction(-1, 1)
  }

  def do_/(that: Fraction): Fraction = {
    val that_s = that.simplify()
    val this_s = this.simplify()
    this_s * Fraction(that_s.fenmu, that_s.fenzi)
  }
  def do_===(that: String): Boolean = {
    this.toString == that
  }
}

object Fraction {
  def apply(nume: Int, deno: Int): Fraction = Fraction((nume, deno)).simplify()
  def apply(num:  Int): Fraction = Fraction(num, 1)
  def apply(str: String): Fraction = {
    val parts = str.split("/")
    require(parts.length == 2, "Invalid fraction format")
    val (numerator, denominator) = (parts(0).toInt, parts(1).toInt)
    require(denominator != 0, "Denominator cannot be zero")
    Fraction(numerator, denominator).simplify()
  }

}
