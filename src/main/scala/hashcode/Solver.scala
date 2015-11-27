package hashcode

import scala.annotation.tailrec

object Solver {
  val possibleOrientations = List(
    Orientation(1, 3), Orientation(3, 1), Orientation(1, 4), Orientation(4, 1), Orientation(2, 2), Orientation(1, 5), Orientation(5, 1), Orientation(1, 6), Orientation(6, 1),
    Orientation(2, 3), Orientation(3, 2), Orientation(1, 7), Orientation(7, 1), Orientation(1, 8), Orientation(8, 1), Orientation(2, 4), Orientation(4, 2),
    Orientation(1, 9), Orientation(9, 1), Orientation(3, 3),
    Orientation(1, 10), Orientation(10, 1), Orientation(2, 5), Orientation(5, 2), Orientation(1, 11), Orientation(11, 1),
    Orientation(1, 12), Orientation(12, 1), Orientation(2, 6), Orientation(6, 2), Orientation(3, 4), Orientation(4, 3))

  case class Orientation(rows: Int, cols: Int)
  val orientations = for {
    r <- List(1, 2, 3, 4, 6, 12)
    c = 12 / r
  } yield Orientation(r, c)

  case class Pizza(topleft: Point, botRight: Point, ham: Map[Point, Boolean]) {
    def asSlice = Slice(topleft, botRight)

    def isOk(problem: Problem) = {
      ham.values.count(identity) >= problem.nHam
    }
  }

  def solveBlock(block: Pizza, problem: Problem) = {
    val solutions = for {
      o <- orientations
    } yield {
      val slices = for {
        pizza <- split(block, problem, o)
        if pizza.isOk(problem)
      } yield pizza

      Solution(slices.map(_.asSlice).toList) -> slices.size
    }
    solutions.maxBy(_._2)._1.sol
  }

  def solve2(problem: Problem): Solution = {
    def buildCandidates(remainingPizza: List[List[Boolean]], from: Point): List[Slice] =
      for {
        or <- possibleOrientations
        slice = Slice(from, Point(from.row + or.rows - 1, from.col + or.cols - 1))
        if slice.isValid(problem)
        if slice.points.forall(p => remainingPizza(p.row)(p.col))
        if slice.points.count(p => problem.isHam(p.row, p.col)) == 3
      } yield slice

    def updateRemaining(remainingPizza: List[List[Boolean]], slice: Slice): List[List[Boolean]] =
      for {
        (row, i) <- remainingPizza.zipWithIndex
      } yield for {
        (b, j) <- row.zipWithIndex
        newB = b && !slice.has(Point(i, j))
      } yield newB

    def next(p: Point) =
      if (p.col == problem.nbCols - 1)
        if (p.row + 1 < problem.nbRows)
          Some(Point(p.row + 1, 0))
        else
          None
      else
        Some(Point(p.row, p.col + 1))

    def solveRec(remainingPizza: List[List[Boolean]], slices: List[Slice], last: Point = Point(0, 0)): List[Slice] = {

      val candidates = for {
        i <- last.row until problem.nbRows
        j <- last.col until problem.nbCols
      } yield Point(i, j)
      candidates.find(c => remainingPizza(c.row)(c.col)) match {
        case Some(topLeftFree) =>
          val candidateSlices = buildCandidates(remainingPizza, topLeftFree)
          if (candidateSlices.isEmpty)
            next(topLeftFree) match {
              case Some(nextPoint) =>
                solveRec(
                  updateRemaining(remainingPizza, Slice(topLeftFree, topLeftFree)),
                  slices,
                  nextPoint)
              case None => slices
            }
          else {
            val slice = candidateSlices.maxBy(s => s.size)
            solveRec(
              updateRemaining(remainingPizza, slice),
              slice :: slices,
              next(topLeftFree).get)
          }
        case None => slices
      }
    }

    val blocks = split(problem, Orientation(12, 12))
    val initSlices = blocks.flatMap(solveBlock(_, problem))
    val usedPoints = initSlices.flatMap(_.points)
    val rest = for {
      i <- (0 until problem.nbRows).toList
    } yield for {
      j <- (0 until problem.nbCols).toList
      p = Point(i, j)
      used = usedPoints.contains(p)
    } yield !used
    val slices = solveRec(rest, Nil)
    Solution(initSlices ::: slices)

  }

  def split(block: Pizza, problem: Problem, orientation: Orientation): List[Pizza] = {
    println(s"splitting $block")
    val pizzas = for {
      row <- block.topleft.row until block.botRight.row by orientation.rows
      col <- block.topleft.col until block.botRight.col by orientation.cols
    } yield {
      val rowEnd = orientation.rows + row - 1
      val colEnd = orientation.cols + col - 1
      println(s"$row to $rowEnd x $col to $colEnd")
      val hams = (for {
        i <- row to rowEnd
        j <- col to colEnd
        ham = problem.pizza(i)(j) == 'H'
      } yield Point(i, j) -> ham).toMap

      Pizza(Point(row, col), Point(rowEnd, colEnd), hams)
    }
    pizzas.toList
  }

  def split(problem: Problem, orientation: Orientation): List[Pizza] =
    {
      val pizzas = for {
        row <- 0 until problem.nbRows by orientation.rows
        col <- 0 until problem.nbCols by orientation.cols
      } yield {
        val rowEnd = orientation.rows + row - 1
        val colEnd = orientation.cols + col - 1
        val hams = (for {
          i <- row to rowEnd
          j <- col to colEnd
          ham = problem.isHam(i, j)
        } yield Point(i, j) -> ham).toMap

        Pizza(Point(row, col), Point(rowEnd, colEnd), hams)
      }
      pizzas.toList
    }

  def solve3(problem: Problem): Solution = {

    def allPossibleSlices(problem: Problem): List[Slice] = {
      val possibleSlices = for {
        row <- 0 until problem.nbRows
        col <- 0 until problem.nbCols
      } yield {
        val allSlices = possibleOrientations.map(orientation => Slice(Point(row, col), Point(row + orientation.rows - 1, col + orientation.cols - 1)))
        allSlices.filter { slice => slice.isValid(problem) }.filter { slice => slice.nbHams(problem) >= problem.nHam }
      }
      possibleSlices.toList.flatten
    }

    def constructPointToSlices(problem: Problem, slices: List[Slice]): List[Map[Point, List[Slice]]] = {
      val pointInSlices = for {
        slice <- slices
        row <- slice.p1.row to slice.p2.row
        col <- slice.p1.col to slice.p2.col
      } yield {
        (Point(row, col), slice)
      }
      pointInSlices.groupBy(_._1).map { case (point, pointSlice) => (point, pointSlice.map(_._2)) }.groupBy(_._2.size).toList.sortWith { case ((size1, _), (size2, _)) => size1 <= size2 }.map { case (size, pointSlices) => pointSlices }
    }

    def selectCriticalSlices(criticalSlices: List[Slice], lessCriticalSlices: Map[Point, List[Slice]]): List[Slice] = {
      if (lessCriticalSlices.isEmpty) {
        criticalSlices
      } else {
        val lessCriticalSlicesNoScoring = lessCriticalSlices.map { case (point, pointSlice) => pointSlice }.foldLeft(List.empty[Slice])(_ ::: _)
        val lessCriticalSlicesNoScoringFiltering = lessCriticalSlicesNoScoring.filter { sl => criticalSlices.contains(sl) }
        if (lessCriticalSlicesNoScoringFiltering.isEmpty) {
          criticalSlices
        } else {
          val criticalSlicesWithScoring = lessCriticalSlicesNoScoringFiltering.groupBy(sl => lessCriticalSlicesNoScoringFiltering.count(_.equals(sl)))
          criticalSlicesWithScoring.get(criticalSlicesWithScoring.keysIterator.max).get.toSet.toList
        }
      }
    }

    def selectCriticalSlicesInit(pointToSlices: List[Map[Point, List[Slice]]]): List[Slice] = {
      if (pointToSlices.isEmpty) {
        List.empty
      } else {
        val criticalPointsSlices = pointToSlices(0)
        val criticalSlicesNoScoring = criticalPointsSlices.map { case (point, pointSlice) => pointSlice }.foldLeft(List.empty[Slice])(_ ::: _)
        selectCriticalSlices(criticalSlicesNoScoring, criticalPointsSlices).toSet.toList
      }
    }

    def selectMostCriticalSlices(pointToSlices: List[Map[Point, List[Slice]]]): List[Slice] = {

      def selectMostCriticalSlicesRec(pointToSlices: List[Map[Point, List[Slice]]], criticalSlices: List[Slice]): List[Slice] = {
        pointToSlices match {
          case head :: tail =>
            val mostCriticalSlices = selectCriticalSlices(criticalSlices, head)
            if (mostCriticalSlices.length == 1) {
              mostCriticalSlices
            } else {
              selectMostCriticalSlicesRec(tail, mostCriticalSlices)
            }
          case _ => criticalSlices
        }
      }

      selectMostCriticalSlicesRec(pointToSlices.drop(1), selectCriticalSlicesInit(pointToSlices))
    }

    def updateSliceList(slices: List[Slice], slice: Slice): List[Slice] = {
      slices.filter { sl => !sl.overlaps(slice) }
    }

    def selectBestMostCriticalSlices(problem: Problem, slices: List[Slice], criticalSlices: List[Slice]): Slice = {
      if (criticalSlices.length == 1) {
        criticalSlices(0)
      } else {
        criticalSlices(1)
      }
    }

    def chooseMostCriticalSlices(problem: Problem, slices: List[Slice]): List[Slice] = {

      def chooseMostCriticalSlicesRec(problem: Problem, slices: List[Slice], chooseSlices: List[Slice]): List[Slice] = {
        if (slices.isEmpty) {
          chooseSlices
        } else {
          val pointToSlices = constructPointToSlices(problem, slices)
          val mostCriticalSlices = selectMostCriticalSlices(pointToSlices)
          val chooseSlice = selectBestMostCriticalSlices(problem, slices, mostCriticalSlices)
          chooseMostCriticalSlicesRec(problem, updateSliceList(slices, chooseSlice), chooseSlice :: chooseSlices)
        }
      }

      chooseMostCriticalSlicesRec(problem, slices, List.empty)
    }

    val slices = chooseMostCriticalSlices(problem, allPossibleSlices(problem))
    Solution(slices)
  }

  def solve(problem: Problem) = {
    case class FoldState(ham: Int, slices: List[Slice])
    def initialState(row: Int, height: Int) = FoldState(0, List(Slice(Point(row, 0), Point(row + height - 1, -1))))

    def splitLines(lines: List[String], row: Int) = {
      val state = lines.transpose.foldLeft(initialState(row, lines.length)) { (state, elems) ⇒
        val (cur :: tail) = state.slices
        val np2 = cur.p2.copy(col = cur.p2.col + 1)
        val ham = elems.count('H'.==)
        val size = cur.size + elems.length
        if (state.ham >= problem.nHam && (ham > 0 || size > problem.maxCells)) {
          //start new slice
          //            print(s")($elem")
          FoldState(ham, Slice(cur.p1.copy(col = np2.col), np2) :: state.slices)
        } else if (size > problem.maxCells) {
          // slide current slice to right
          val np1 = cur.p1.copy(col = cur.p1.col + 1)
          val removedHam = (row until row + elems.length).count(problem.isHam(_, cur.p1.col))
          //            print(s"$elem")
          tail match {
            case prev :: tail if prev.size + elems.length <= problem.maxCells ⇒
              // increase previous slice
              FoldState(state.ham + ham - removedHam,
                cur.copy(p1 = np1, np2) :: prev.copy(p2 = prev.p2.copy(col = prev.p2.col + 1)) :: tail)
            case _ ⇒
              // waste some elements
              FoldState(state.ham + ham - removedHam,
                cur.copy(p1 = np1, np2) :: tail)
          }
        } else {
          //            print(s"$elem")
          FoldState(state.ham + ham, cur.copy(p2 = np2) :: tail)
        }
      }

      if (state.ham >= problem.nHam) state.slices
      else state.slices.tail match {
        case head :: tail ⇒
          val ncol = (head.p2.col + (problem.maxCells - head.size) / lines.length) min state.slices.head.p2.col
          head.copy(p2 = head.p2.copy(col = ncol)) :: tail
        case _ ⇒ Nil
      }

    }

    def optimalSplitting(problem: Problem): List[Slice] = {

      case class ScorePointer(previousRow: Int, score: Int)

      def computeScore(problem: Problem, from: Int, to: Int): Int = {
        splitLines(problem.pizza.slice(from, to), from).map(slice => slice.size).sum
      }

      def constructScore(problem: Problem, splits: List[ScorePointer]): List[ScorePointer] = {
        val currentRow = splits.size
        if (problem.nbRows == currentRow - 1) {
          splits
        } else {
          val interestedSplits = splits.takeRight(6)
          val scoreSplits = interestedSplits.zipWithIndex.map({ case (split, index) => split.score + computeScore(problem, currentRow - interestedSplits.size + index, currentRow) })
          val optimalSplit = scoreSplits.zipWithIndex.maxBy(_._1)
          val selectedSplit = ScorePointer(currentRow - interestedSplits.size + optimalSplit._2, optimalSplit._1)
          constructScore(problem, splits ::: List(selectedSplit))
        }
      }

      def callBack(splits: List[ScorePointer], result: List[Int]): List[Int] = {
        val lastSplit = splits.last
        if (lastSplit.previousRow == -1) {
          result
        } else {
          callBack(splits.take(lastSplit.previousRow + 1), splits.size - lastSplit.previousRow - 1 :: result)
        }
      }

      def executeSplit(problem: Problem, splits: List[Int], row: Int): List[Slice] = {
        splits match {
          case head :: tail => splitLines(problem.pizza.slice(row, row + head), row) ::: executeSplit(problem, tail, row + head)
          case _            => List.empty
        }
      }

      val constructList = constructScore(problem, List(ScorePointer(-1, 0)))
      val splitList = callBack(constructList, List.empty)
      executeSplit(problem, splitList, 0)
    }

    def findMissingpoint(problem: Problem, slices: List[Slice]): List[Point] = {

      def findMissingPointRec(problem: Problem, slices: List[Slice], currentPoint: Point, missingPoints: List[Point]): List[Point] = {
        if (currentPoint.row == problem.nbRows) {
          missingPoints
        } else {
          val slice = Slice(currentPoint, currentPoint)
          if (slice.overlapsAll(slices)) {
            if (slice.p2.col == problem.nbCols - 1) {
              findMissingPointRec(problem, slices, Point(slice.p1.row + 1, 0), missingPoints)
            } else {
              findMissingPointRec(problem, slices, Point(slice.p1.row, slice.p2.col + 1), missingPoints)
            }
          } else {
            if (slice.p2.col == problem.nbCols - 1) {
              findMissingPointRec(problem, slices, Point(slice.p1.row + 1, 0), currentPoint :: missingPoints)
            } else {
              findMissingPointRec(problem, slices, Point(slice.p1.row, slice.p2.col + 1), currentPoint :: missingPoints)
            }
          }
        }
      }

      findMissingPointRec(problem, slices, Point(0, 0), List.empty)
    }

    def correctedOptimalSplitting(problem: Problem): List[Slice] = {

      def findValidSlice(problem: Problem, slices: List[Slice], point: Point): Option[Slice] = {
        val candidateSlices = possibleOrientations.map(orientation => Slice(point, Point(point.row + orientation.rows - 1, point.col + orientation.cols - 1)))
        val validSlices = candidateSlices.filter(slice => slice.isValid(problem)).filter(slice => slice.nbHams(problem) >= problem.nHam).filter(slice => !slice.overlapsAll(slices))
        if (validSlices.isEmpty) {
          None
        } else {
          Some(validSlices.maxBy { slice => slice.size })
        }
      }

      def findNewValidSliceRec(problem: Problem, slices: List[Slice], missingPoints: List[Point]): List[Slice] = {
        missingPoints match {
          case head :: tail => findValidSlice(problem, slices, head) match {
            case Some(slice) => findNewValidSliceRec(problem, slice :: slices, missingPoints diff List(head))
            case None        => findNewValidSliceRec(problem, slices, missingPoints diff List(head))
          }
          case _ => slices
        }
      }

      def extendSliceVerticallyRec(problem: Problem, slices: List[Slice], slice: Slice): List[Slice] = {
        val candidateSlices = List(Slice(Point(slice.p1.row - 1, slice.p1.col), slice.p2), Slice(slice.p1, Point(slice.p2.row + 1, slice.p2.col)))
        val validSlices = candidateSlices.filter(sl => sl.isValid(problem)).filter(sl => sl.size <= problem.maxCells).filter(sl => !sl.overlapsAll(slices diff List(slice)))
        if (validSlices.isEmpty) {
          List.empty
        } else {
          validSlices ::: validSlices.map(sl => extendSliceVerticallyRec(problem, slices diff List(slice), sl)).flatten
        }
      }

      def extendSliceVertically(problem: Problem, slices: List[Slice], slice: Slice): Option[Slice] = {
        val extendSlices = extendSliceVerticallyRec(problem, slices, slice)
        if (extendSlices.isEmpty) {
          None
        } else {
          Some(extendSlices.maxBy { slice => slice.size })
        }
      }

      def extendSlideRec(problem: Problem, slices: List[Slice], slicesLeft: List[Slice]): List[Slice] = {
        slicesLeft match {
          case head :: tail =>
            extendSliceVertically(problem, slices, head) match {
              case Some(slice) => extendSlideRec(problem, slice :: slices diff List(head), slicesLeft diff List(head))
              case None        => extendSlideRec(problem, slices, slicesLeft diff List(head))
            }
          case _ => slices
        }
      }

      val slices = optimalSplitting(problem)
      val missingPoints = findMissingpoint(problem, slices)
      val newSlices = findNewValidSliceRec(problem, slices, missingPoints)
      extendSlideRec(problem, newSlices, newSlices)
    }

    val slices = correctedOptimalSplitting(problem)
    Solution(slices)
  }
}