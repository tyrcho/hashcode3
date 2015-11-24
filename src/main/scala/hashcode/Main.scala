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


  def display(problem: Problem, solution: Solution) = {
    val res = solution.sol.foldLeft(problem.pizza) { (pizza,slice) ⇒
      val sol1 = pizza.updated(slice.p1.row, pizza(slice.p1.row).updated(slice.p1.col, '['))
      sol1.updated(slice.p2.row, sol1(slice.p2.row).updated(slice.p2.col, ']'))
    }
    println(res.mkString("\n"))
  }
}