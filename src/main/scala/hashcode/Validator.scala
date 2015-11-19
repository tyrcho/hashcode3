package hashcode

import scala.util.Try

object Validator {

  def score(solution: Solution, problem: Problem): Try[Int] = {
    def validateSlice(slice: Slice) = {
      assert(slice.p1.row <= slice.p2.row, s"Incorrect slice: $slice")
      assert(slice.p1.col <= slice.p2.col, s"Incorrect slice: $slice")
      assert(slice.p1.row >= 0 && slice.p2.row < problem.nbRows, s"Incorrect slice: $slice")
      assert(slice.p1.col >= 0 && slice.p2.col < problem.nbCols, s"Incorrect slice: $slice")
      assert( sliceSize(slice) <= problem.maxCells, s"slice $slice is too big")
      val pSlice = problem.pizza.slice(slice.p1.row, slice.p2.row+1) map { _.slice(slice.p1.col, slice.p2.col+1) }
      val nHam = pSlice.flatten.count('H'.==)
      assert(nHam >= problem.nHam, s"slice $slice contains $nHam, expected ${problem.nHam}")
    }

    Try {
      solution.sol foreach validateSlice
      solution.sol.combinations(2) foreach { case List(s1,s2) ⇒ checkNonOverlap(s1,s2) }
      (solution.sol map sliceSize).sum
    }

  }

  def sliceSize(slice: Slice): Int = {
    (slice.p2.row - slice.p1.row + 1) * (slice.p2.col - slice.p1.col + 1)
  }
  def checkNonOverlap(slice1: Slice, slice2:Slice) = {
    assert(slice2.p2.row < slice1.p1.row || slice1.p2.row < slice2.p1.row || slice2.p2.col < slice1.p1.col || slice1.p2.col < slice2.p1.col,
    s"Slice $slice1 and slice $slice2 overlaps")
  }
}