package hashcode

case class Point(row: Int, col:Int) {
  override def toString = s"$row $col"
}
case class Slice(p1: Point, p2: Point) {
  override def toString = s"$p1 $p2"
  def overlaps(o:Slice) = !(o.p2.row < p1.row || p2.row < o.p1.row || o.p2.col < p1.col || p2.col < o.p1.col)
  def size = (p2.row - p1.row + 1) * (p2.col - p1.col + 1)
}
case class Solution(sol: List[Slice])