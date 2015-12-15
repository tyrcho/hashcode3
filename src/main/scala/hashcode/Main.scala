package hashcode

import scala.util.{ Failure, Success }

object Main extends App {
  val problem = Parser.read()
  val solution = Solver.solve3(problem)
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
    
    def constructPointMap(slices: List[Slice], sliceColors: Map[Slice, String], pointColors: Map[Point,String]) : Map[Point,String] = {
      slices match {
        case head :: tail =>
          val sliceColor = sliceColors.get(head).get
          val coloredPoints = for {
            row <- head.p1.row to head.p2.row
            col <- head.p1.col to head.p2.col
          }yield{
            (Point(row,col),sliceColor)
          }
          constructPointMap(tail, sliceColors, pointColors ++ coloredPoints.toMap)
        case _ => pointColors
      }
    }
    
    def colorProblem(problem: Problem, pointColors: Map[Point, String]) : List[String] = {
      val colorLines = for {
        row <- 0 until problem.nbRows
      }yield{
        val line = problem.pizza(row)
        val colorCharacters = for {
          col <- 0 until problem.nbCols
        }yield{
          pointColors.getOrElse(Point(row,col), Console.WHITE) + problem.pizza(row)(col) + Console.RESET
        }
        colorCharacters.toList.reduce(_+_)
      }
      colorLines.toList
    }

    val sliceColors = constructMap(solution.sol, Map.empty)
    val pointColors= constructPointMap(solution.sol, sliceColors, Map.empty)
    val problemColor = colorProblem(problem,pointColors)
    println(problemColor.mkString("\n").replace('T', ' '))
  }
}