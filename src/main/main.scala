import sfraction._
import sfraction.HasFraction._
import scala.math.Numeric.Implicits._
object Main extends App {
  val checkList = List(
    (1.F).toString -> "1",
    (-2.F).toString -> "-2",
    (1.F / 2.F).toString -> "(1/2)",

    ("-1/2".F == "1/-2".F) -> true,
    ("-1/2".F == "-1/2".F) -> true,
    ("-1/2".F == "1/2".F) -> false,
    ("1/2".F == "1/-2".F) -> false,
    ("1/2".F == "-1/2".F) -> false,
    ("1/2".F == "1/2".F) -> true,

    ("-1/2".F > "1/-2".F) -> false,
    ("-1/2".F > "-1/2".F) -> false,
    ("-1/2".F > "1/2".F) -> false,
    ("1/2".F > "1/-2".F) -> true,
    ("1/2".F > "-1/2".F) -> true,
    ("1/2".F > "1/2".F) -> false,

    ("-1/2".F >= "1/-2".F) -> true,
    ("-1/2".F >= "-1/2".F) -> true,
    ("-1/2".F >= "1/2".F) -> false,
    ("1/2".F >= "1/-2".F) -> true,
    ("1/2".F >= "-1/2".F) -> true,
    ("1/2".F >= "1/2".F) -> true,
  )

  println("====start====")
  println((1.F) === "1")
  def checkonce(t: (Any, Any)) = {
    if (t._1 == t._2) {
      println("[-]")
    } else {
      println("[x]"+t._1)
    }
  }
  checkList.foreach((i) => {
    checkonce(i)
  })

}
