package sfraction

object HasFraction {
  implicit class IntToFractionExtension(private val i: Int) extends AnyVal {
    def toFraction: Fraction = Fraction(i, 1)
    def F:          Fraction = toFraction
    def ===(that: Fraction): Boolean = that == i.F
  }
  implicit class TupleToFractionExtension(private val t: (Int, Int)) extends AnyVal {
    def toFraction: Fraction = Fraction(t._1, t._2)
    def F:          Fraction = toFraction
  }
  implicit class StringToFractionExtension(private val s: String) extends AnyVal {
    def toFraction: Fraction = Fraction(s)
    def F:          Fraction = toFraction
  }
  implicit class FractionToFractionExtension(private val f: Fraction) extends AnyVal {
    def toFraction: Fraction = f
    def F:          Fraction = toFraction
  }

  import scala.language.implicitConversions
  implicit def stringToFraction(str: String): Fraction = Fraction(str)
  implicit def intToFraction(value:  Int):    Fraction = Fraction(value, 1)
}
