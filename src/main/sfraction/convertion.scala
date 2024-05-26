package sfraction

object HasFraction {
  implicit class IntToFractionExtension(private val i: Int) extends AnyVal {
    def toFraction: Fraction = Fraction(i, 1)
    def F:          Fraction = toFraction
  }
  implicit class TupleToFractionExtension(private val t: (Int, Int)) extends AnyVal {
    def toFraction: Fraction = Fraction(t._1, t._2)
    def F:          Fraction = toFraction
  }
  implicit class StringToFractionExtension(private val s: String) extends AnyVal {
    def toFraction: Fraction = Fraction(s)
    def F:          Fraction = toFraction
  }

  import scala.language.implicitConversions
  implicit def stringToFraction(str: String): Fraction = Fraction(str)
  implicit def intToFraction(value:  Int):    Fraction = Fraction(value, 1)
}
