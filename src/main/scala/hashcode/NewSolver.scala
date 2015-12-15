package hashcode

import scala.annotation.tailrec
import scala.util.Random

object NewSolver {
  def solve(problem: Problem): Solution = {
    val possibleSlices = allPossibleSlices(problem)
    println(s"${possibleSlices.size} possibleSlices")
    val slicesPerPoint = initialSlicesPerPoint(possibleSlices, problem)
    println(s"${slicesPerPoint.size} slicesPerPoint")
    val overlapping = overlappingSlices(slicesPerPoint)
    println(s"${overlapping.size} overlapping")
    val initialSolution = PartialSolution(possibleSlices, Set.empty, slicesPerPoint, overlapping)
    Solution(initialSolution.chooseAll.toList)
  }

  case class PartialSolution(
      undecidedSlices: Set[Slice],
      chosenSlices: Set[Slice],
      slicesPerPoint: Map[Point, Set[Slice]],
      overlappingSlices: Map[Slice, Set[Slice]]) {

    def chooseAll: Set[Slice] =
      if (undecidedSlices.isEmpty) chosenSlices
      else choseSlice.chooseAll

    def choseSlice: PartialSolution = {
      val slice = undecidedSlices.minBy(scoredSlices)
      println(s"chosing slice $slice")
      val invalidated = overlappingSlices(slice)
      val delta = invalidated + slice
      copy(
        undecidedSlices = undecidedSlices -- delta,
        slicesPerPoint = slicesPerPoint.filterNot(_._2.contains(slice)),
        chosenSlices = chosenSlices + slice)
    }

    def scoredSlices =
      undecidedSlices.map { slice =>
        val overlapping = overlappingSlices(slice)
        println(s"evaluation $slice : ${overlapping.size} overlapping")
        val total = (for {
          overlapper <- overlapping
          if undecidedSlices.contains(overlapper)
          point <- overlapper.points
          slicesWithPoint = slicesPerPoint(point) diff overlapping
          score = if (slicesWithPoint.size == 0) 1 else 0
        } yield score).sum
        slice -> total
      }.toMap

  }

  def overlappingSlices(slicesPerPoint: Map[Point, Set[Slice]]): Map[Slice, Set[Slice]] = {
    slicesPerPoint.foldLeft(Map.empty[Slice, Set[Slice]].withDefaultValue(Set.empty)) {
      case (map, (point, slices)) =>
        slices.foldLeft(map) {
          case (map, slice) =>
            map.updated(slice, map(slice) ++ slices - slice)
        }
    }
  }

  def initialSlicesPerPoint(allSlices: Set[Slice], problem: Problem): Map[Point, Set[Slice]] = {
    allSlices.foldLeft(Map.empty[Point, Set[Slice]].withDefaultValue(Set.empty)) {
      case (map, slice) =>
        slice.points.foldLeft(map) {
          case (map, point) =>
            map.updated(point, map(point) + slice)
        }
    }
  }

  def allPossibleSlices(problem: Problem): Set[Slice] = {
    for {
      row <- 0 until problem.nbRows
      col <- 0 until problem.nbCols
      orientation <- possibleOrientations
      slice = Slice(Point(row, col), Point(row + orientation.rows - 1, col + orientation.cols - 1))
      if slice.isValid(problem)
      if slice.nbHams(problem) >= problem.nHam
    } yield slice
  }.toSet

  val possibleOrientations = List(
    Orientation(1, 3), Orientation(3, 1), Orientation(1, 4), Orientation(4, 1), Orientation(2, 2), Orientation(1, 5), Orientation(5, 1), Orientation(1, 6), Orientation(6, 1),
    Orientation(2, 3), Orientation(3, 2), Orientation(1, 7), Orientation(7, 1), Orientation(1, 8), Orientation(8, 1), Orientation(2, 4), Orientation(4, 2),
    Orientation(1, 9), Orientation(9, 1), Orientation(3, 3),
    Orientation(1, 10), Orientation(10, 1), Orientation(2, 5), Orientation(5, 2), Orientation(1, 11), Orientation(11, 1),
    Orientation(1, 12), Orientation(12, 1), Orientation(2, 6), Orientation(6, 2), Orientation(3, 4), Orientation(4, 3))

  case class Orientation(rows: Int, cols: Int)
}