package hashcode

import scala.util.Try

object Validator {

  def score(solution: Solution, problem: Problem): Try[Int] = {
    def validateSlice(slice: Slice) = {
      assert(slice.p1.row <= slice.p2.row, s"Incorrect slice: $slice")
      assert(slice.p1.col <= slice.p2.col, s"Incorrect slice: $slice")
      assert( (slice.p2.row - slice.p1.row +1) * (slice.p2.col - slice.p2.col) <= problem.maxCells, s"slice $slice is too big")
      val pSlice = problem.pizza.slice(slice.p1.row, slice.p2.row+1) map { _.slice(slice.p1.col, slice.p2.col+1) }
      val nHam = pSlice.flatten.count('H'.==)
      assert(nHam >= problem.nHam, s"slice $slice contains $nHam, expected ${problem.nHam}")
    }

    Try {
      solution.sol foreach validateSlice
      42
    }

  }

}