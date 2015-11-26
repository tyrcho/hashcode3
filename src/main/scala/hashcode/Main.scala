package hashcode

import scala.util.{ Failure, Success }

object Main extends App {
  val problem = Parser.read()
  val solution = Solver.solve(problem)
  display(problem, solution)
  Validator.score(solution, problem) match {
    case Success(score) =>
      println(s"score : $score")
      Formatter.write(solution, score)
    case Failure(e) =>
      e.printStackTrace()
      Formatter.write(solution, 0)
  }

  lazy val colors = Iterator.continually(Iterator(Console.GREEN_B, Console.YELLOW_B, Console.BLUE_B, Console.RED_B, Console.MAGENTA_B, Console.CYAN_B)).flatten

  def display(problem: Problem, solution: Solution) = {

    def constructMap(slices: List[Slice], sliceColors: Map[Slice, String]): Map[Slice, String] = {
      slices match {
        case head :: tail =>
          constructMap(tail, sliceColors updated (head, colors.next()))
        case _ => sliceColors
      }
    }

    def addColorPoint(problem: Problem, point: Point, slices: List[Slice], sliceColors: Map[Slice, String]): String = {
      val slice = Slice(point, point)
      if (slice.overlapsAll(slices)) {
        val firstOverlap = slice.overlapFirst(slices)
        sliceColors.get(firstOverlap).get + problem.pizza(point.row)(point.col) + Console.RESET
      } else {
        Console.WHITE + problem.pizza(point.row)(point.col) + Console.RESET
      }
    }

    def addColorLine(problem: Problem, row: Int, slices: List[Slice], sliceColors: Map[Slice, String]) : String = {
      List.range(0,problem.nbCols,1).map(i => addColorPoint(problem,Point(row,i),slices,sliceColors)).reduce(_ +_)
    }

    val sliceColors = constructMap(solution.sol, Map.empty)
    println(List.range(0,problem.nbRows,1).map(row => addColorLine(problem,row,solution.sol,sliceColors)).mkString("\n").replace('T', ' '))
  }
}