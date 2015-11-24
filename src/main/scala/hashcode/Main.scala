package hashcode

import scala.util.{Failure, Success}

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

  lazy val colors = Iterator.continually(Iterator(Console.BLUE_B, Console.RED_B, Console.MAGENTA_B, Console.CYAN_B)).flatten

  def display(problem: Problem, solution: Solution) = {
    def addColor(str: String, color: String, from: Int, to: Int) = {
      val (before, right) = str.splitAt(from)
      val (mid, after) = right.splitAt(to-from+1)
      before + color + mid + Console.RESET + after
    }

    val res = solution.sol.foldLeft(problem.pizza) { (pizza,slice) ⇒
      val color = colors.next()
      (slice.p1.row to slice.p2.row).foldLeft(pizza) { (pizza, r) ⇒
        pizza.updated(r, addColor(pizza(r), color, slice.p1.col, slice.p2.col))
      }
    }

    println(res.mkString("\n").replace('T', ' '))
  }
}