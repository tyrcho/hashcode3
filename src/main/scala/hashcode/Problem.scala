package hashcode

case class Server(size: Int, capacity: Int, id: Int) {
  def ratio: Float = capacity / size.toFloat
}

case class Problem(nbRows: Int, nbCols: Int, nHam: Int, maxCells:Int , pizza: List[String])

