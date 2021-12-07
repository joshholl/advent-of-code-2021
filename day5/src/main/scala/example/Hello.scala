package example
import scala.io.Source._
import scala.annotation.tailrec

object Hello {
  val testLines = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"""
  case class Point(x: Int, y: Int)

  def toPoint(str: String): Point = {
    val parts = str.split(",").map(x => x.toInt)
    Point(parts(0), parts(1))
  }

  def diffAsNRange(start: Int, end: Int): List[Int] =
    (start.min(end) to start.max(end)).toList

  def invertIfNegative(diff: Int)(list: List[Int]) =
    if (diff < 0) list.reverse else list

  def pointsBetween(start: Point, end: Point): List[Point] = {
    return if (start.x == end.x) {
      diffAsNRange(start.y, end.y).map(y => Point(start.x, y))
    } else if (start.y == end.y) {
      diffAsNRange(start.x, end.x).map(x => Point(x, start.y))
    } else {
      val xDiff = end.x - start.x
      val yDiff = end.y - start.y
      val xOffsets = invertIfNegative(xDiff)(diffAsNRange(xDiff, 0))
      val yOffsets = invertIfNegative(yDiff)(diffAsNRange(yDiff, 0))

      xOffsets.zipWithIndex.map { case (value, index) =>
        Point(start.x + value, start.y + yOffsets(index))
      }.toList
    }
  }

  def toPointPair(line: String): (Point, Point) = {
    val parts = line.split("->").map(x => x.trim()).map(toPoint).toList
    return (parts(0), (parts(1)))
  }

  def main(args: Array[String]): Unit = {
    val input =
      (if (args.isEmpty) testLines.split("\n").toList
       else fromFile(args(0)).getLines.toList)
    val lines = input.map(toPointPair)
    val part1Points = lines
      .filter { case (start, end) => start.y == end.y || start.x == end.x }
      .map { case (start, end) => pointsBetween(start, end) }
    val part2Points = lines.map { case (start, end) =>
      pointsBetween(start, end)
    }

    val part1 = crossOvers(0, part1Points, List.empty)
    val part2 = crossOvers(0, part2Points, List.empty)

    println(s"Part 1 =  $part1")
    println(s"Part 2 = $part2")
  }

  def crosses(a: List[Point], b: List[Point]): List[Point] = {
    return if (a == b) List.empty else a.intersect(b)
  }

  def crossOvers(
      count: Int,
      lines: List[List[Point]],
      alreadyCrossed: List[Point]
  ): Int = {

    if (lines.isEmpty)
      return count
    val currentLine = lines.head
    val pointsCrossed = lines
      .flatMap(x => crosses(currentLine, x))
      .distinct
      .filter(p => !alreadyCrossed.contains(p))

    return crossOvers(
      count + pointsCrossed.length,
      lines.drop(1),
      alreadyCrossed.appendedAll(pointsCrossed)
    )

  }
}
