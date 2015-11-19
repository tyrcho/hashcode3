package hashcode

case class Point(row: Int, col:Int) {
  override def toString = s"$row $col"
}
case class Slice(p1: Point, p2: Point) {
  override def toString = s"${p1.row} ${p2.row} ${p1.col} ${p2.col}"
}
case class Solution(sol: List[Slice])