package hashcode

object Parser {
  def read(): Problem = {
    val input = io.Source.fromFile("input/data.in.txt").getLines().toList
    val first = input.head
    val Array(nbRows, nbCols, h, s) = first.split(" ").map(_.toInt)

    Problem(nbRows, nbCols, h, s, input.tail)
  }
  
}