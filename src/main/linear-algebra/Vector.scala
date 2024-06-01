package linearAlgebra

import sfraction._
import sfraction.HasFraction._

class Vector(row: Array[Fraction]) extends Matrix(Array(row)) {
  require(numCols == 1, "[Vector] must be a column matrix.")
}
