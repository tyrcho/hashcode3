package hashcode

object Solver {

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

  def solve(problem: Problem): Solution = {
    val blocks = split(problem, Orientation(12, 12))
    Solution(blocks.flatMap(solveBlock(_, problem)))
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
          ham = problem.pizza(i)(j) == 'H'
        } yield Point(i, j) -> ham).toMap

        Pizza(Point(row, col), Point(rowEnd, colEnd), hams)
      }
      pizzas.toList
    }
}