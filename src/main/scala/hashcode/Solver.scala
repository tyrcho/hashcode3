package hashcode

object Solver {

  case class Orientation(rows: Int = 12, cols: Int = 1)
  case class Pizza(topleft: Point, botRight: Point, ham: Map[Point, Boolean]) {
    def asSlice = Slice(topleft, botRight)

    def isOk(problem: Problem) = {
      ham.values.count(identity) >= problem.nHam
    }
  }

  def solve(problem: Problem): Solution = {
    val slices = for {
      pizza <- split(problem, Orientation())
      if pizza.isOk(problem)
    } yield pizza

    Solution(slices.map(_.asSlice).toList)
  }

  def split(problem: Problem, orientation: Orientation): List[Pizza] = {
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