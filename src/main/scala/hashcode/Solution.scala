package hashcode

case class Point(row: Int, col:Int)
case class Slice(p1: Point, p2: Point)
case class Solution(sol: List[Slice])