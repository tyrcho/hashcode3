package hashcode

import scala.util.{Failure, Success}

object Main extends App {
  val problem = Parser.read()
  val solution = Solver.solve(problem)
  Validator.score(solution, problem) match {
    case Success(score) =>
      println(s"score : $score")
      Formatter.write(solution, score)
    case Failure(e) =>
      e.printStackTrace()
      Formatter.write(solution, 0)
  }

}