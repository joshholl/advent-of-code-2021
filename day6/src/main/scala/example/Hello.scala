package example
import scala.io.Source._
import scala.annotation.tailrec

object Day6 {
  val testLines = """3,4,3,1,2"""

  def parseInput(lines: String): List[Long] = {
    lines.split(",").map(x => x.trim().toLong).toList
  }

  @tailrec
  def doDayCycle(day: Int, fish: List[Long]): Long = {
    if (day <= 0) {
      return fish.sum
    }
    return doDayCycle(day - 1, List( fish(1),fish(2),fish(3),fish(4),fish(5),fish(6),fish(7) + fish(0), fish(8), fish(0)))
  }

  def main(args: Array[String]): Unit = {
    val input =
      (if (args.isEmpty) testLines.split("\n").toList
       else fromFile(args(0)).getLines.toList)
    val lines = parseInput(input.head)

    val fishAges = List.fill(9)(0l)

    val start = lines.foldLeft(fishAges)((ages, next) => {
      ages.updated(next.toInt, ages(next.toInt) + 1)

    })

    println(s"Day 80: ${doDayCycle(80, start)}")
    println(s"Day 256: ${doDayCycle(256, start)}")
  }

}
