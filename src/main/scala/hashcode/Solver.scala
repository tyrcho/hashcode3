package hashcode

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

  def solve(problem: Problem) = {
    case class FoldState(ham:Int, slices: List[Slice])
    def initialState(row:Int) = FoldState(0, List(Slice(Point(row, 0), Point(row ,-1))))
    Solution(
      problem.pizza.zipWithIndex flatMap { case (line, row) ⇒
        //        print("(")
        val state = line.foldLeft(initialState(row)) { (state, elem) ⇒
          val (cur :: tail) = state.slices
          val np2 = cur.p2.copy(col = cur.p2.col + 1)
          val isHam = elem == 'H'
          val ham = if (isHam) 1 else 0
          val size = cur.p2.col - cur.p1.col + 1
          if (state.ham == problem.nHam && (isHam ||size == problem.maxCells)) {
            //start new slice
            //            print(s")($elem")
            FoldState(ham, Slice(np2,np2) :: state.slices)
          } else if (size == problem.maxCells) {
            // slide current slice to right
            val np1 = cur.p1.copy(col = cur.p1.col + 1)
            val removedHam = if (problem.isHam(row, cur.p1.col)) 1 else 0
            //            print(s"$elem")
            tail match {
              case prev::tail if prev.size < problem.maxCells ⇒
                // increase previous slice
                FoldState(state.ham + ham - removedHam,
                  cur.copy(p1=np1, np2) :: prev.copy(p2 = cur.p1) :: tail)
              case _        ⇒
                // waste an element
                FoldState(state.ham + ham - removedHam,
                  cur.copy(p1=np1, np2) :: tail)
            }
          } else {
            //            print(s"$elem")
            FoldState(state.ham + ham, cur.copy(p2=np2) :: tail)
          }
        }
        //        if (state.ham < problem.nHam) println() else println(")")
        if (state.ham >= problem.nHam) state.slices else state.slices.tail match {
          case head :: tail ⇒
            val ncol = (head.p2.col + problem.maxCells - head.size) min state.slices.head.p2.col
            head.copy(p2=head.p2.copy(col=ncol)) :: tail
          case _ ⇒ Nil
        }
      }
    )
  }
}