import scala.io.Source._


object Main {
  type Input = (String,Int)
  val testLines = List[String]( "forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

  def main(args: Array[String]): Unit = {
    val lines = if (args.isEmpty) testLines else fromFile(args(0)).getLines
    val steps = lines.map(x => 
      x.split("\\s+") match { case Array(direction, value) => (direction, value.toInt) }
    ).toList
    
    val part1Answer = part1(steps)
    val part2Answer = part2(steps)

    println(s"Part 1: $part1Answer = ${part1Answer._1 * part1Answer._2}")
    println(s"Part 2: $part2Answer = ${part2Answer._1 * part2Answer._2}")
  }

  def part1(list: List[Input]): (Int,Int) = {
    val start: (Int,Int) = (0,0) 
    return list.foldLeft(start)((current, next) => {
      next._1 match {
        case "forward"  => (current._1 + next._2, current._2)
        case "down"           => (current._1,           current._2 + next._2)
        case "up"             => (current._1,           current._2 - next._2)
        case _ => current
      }
    })
  }

  def part2(list: List[Input]): (Int,Int,Int) = {
    val start: (Int,Int,Int) = (0,0,0)
    return list.foldLeft(start)((current,next) => {
      next._1 match {
        case "forward" =>  (current._1 + next._2, current._2 + next._2 * current._3,  current._3)
        case "down" =>     (current._1,           current._2,                         current._3 + next._2)
        case "up" =>       (current._1,           current._2,                         current._3 - next._2)
        case _ => current
      }
    })
  }
}
