package hashcode

import java.io.PrintStream

object Formatter {
  def write(solution: Solution, score: Int): Unit = {
    val name = s"out.${score}.txt"
    val f = new PrintStream(name)
    f.println(solution.sol.length)
    f.println(solution.sol.mkString("\n"))
    f.close
    println(s"wrote to $name")
  }
  
  def read(score: Int): Solution = {
    val input = io.Source.fromFile(s"out.${score}.txt").getLines().toList
    Solution(input.tail.map(line => line.split(" ")).map(chunks => Slice( Point(chunks(0).toInt,chunks(1).toInt), Point(chunks(2).toInt,chunks(3).toInt))))
  }
  
}