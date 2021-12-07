package example
import scala.io.Source._

object Hello {
  val testLines = """16,1,2,0,4,2,7,1,2,14"""

  def distanceBetweenPoints(destination: Int)(start: Int): Int = (destination - start).abs

  def simpleFuelCalc(moves: List[Int]) = moves.sum
  def complexFuelCalc(moves: List[Int]) = moves.map(s => (0 to s).sum).sum

  def parseInput(lines: String): List[Int] = {
    lines.split(",").map(x => x.trim().toInt).toList
  }

  def calculateFuelCosts(startingPoints: List[Int]): (Int, Int) = {
    val destinations = 0 to startingPoints.max
    val fuelCosts = destinations.map(dest => {
        val distances = startingPoints.map(distanceBetweenPoints(dest))
        (
          simpleFuelCalc(distances),
          complexFuelCalc(distances)
        )
    })

    val simple = fuelCosts.minBy(x => x._1)
    val complex = fuelCosts.minBy(x => x._2)
    return (simple._1, complex._2)
  }

  def main(args: Array[String]): Unit = {
    val input =
      (if (args.isEmpty) testLines.split("\n").toList
       else fromFile(args(0)).getLines.toList)
    val lines = parseInput(input.head)

    val (part1, part2) = calculateFuelCosts(lines)

    println(s"Part 1: ${part1}")
    println(s"Part 2: ${part2}")
  }
}
