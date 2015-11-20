package hashcode

case class Point(row: Int, col: Int) {
  override def toString = s"$row $col"
}
case class Slice(p1: Point, p2: Point) {
  override def toString = s"$p1 $p2"

  def has(p: Point) =
    p.row >= p1.row &&
      p.row <= p2.row &&
      p.col >= p1.col &&
      p.col <= p2.col

  def overlaps(o: Slice) = !(o.p2.row < p1.row || p2.row < o.p1.row || o.p2.col < p1.col || p2.col < o.p1.col)

  def size = (p2.row - p1.row + 1) * (p2.col - p1.col + 1)

  def points = for {
    i <- p1.row to p2.row
    j <- p1.col to p2.col
  } yield Point(i, j)

  def isValid(p: Problem) =
    math.max(p1.row, p2.row) < p.nbRows &&
      math.max(p1.col, p2.col) < p.nbCols &&
      math.min(p1.row, p2.row) >= 0 &&
      math.min(p1.col, p2.col) >= 0

}
case class Solution(sol: List[Slice])