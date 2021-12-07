import scala.io.Source._

case class Count(z: Int,o: Int)

object Main {

  def isOn(position: Int)(s: String): Boolean = s(position) == '1'
  def isOff(position: Int)(s: String): Boolean = !isOn(position)(s)

  type In = List[String]


  type Input = String
  val testLines = List[Input](
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  def main(args: Array[String]): Unit = {
    val lines = (if (args.isEmpty) testLines else fromFile(args(0)).getLines).toList

    val part2Answer = part2(lines)
    val part1Answer = part1(lines)
    println(s"Part 1: $part1Answer")
    println(s"Part 2: $part2Answer")
  }

  def fromBinToInt(s: String) = Integer.parseInt(s, 2)

  def part1(list: List[Input]): Int = {

    val opts = List.fill(list(0).length)(Count(0,0))


    val occurrence =  list.flatMap(x => x.toList.zipWithIndex.map{ case (v, i) => (i,v) })
      .foldLeft(opts)((acc, curr) => {
        acc.updated(curr._1, addIt(acc(curr._1))(curr._2))
      })

      val mostCommon = occurrence.foldLeft("")((acc, curr) => curr match {
        case x if (curr.z > curr.o) => acc + "0"
        case _ => acc + 1
      })

      val leastCommon = occurrence.foldLeft("")((acc,curr) => curr match {
        case x if (curr.z < curr.o) => acc + "0"
        case _ => acc + 1
      })


      val mcd = Integer.parseInt(mostCommon, 2)
      val lcd = Integer.parseInt(leastCommon, 2)

      println(mcd)
      println(lcd)

      return mcd * lcd


  }



  def part2(list: List[Input]): Int = {
    val positions = list(0).length
    def incrementOrMax(max: Int)(curr: Int)(l: Int, c:Int): Int =  if (l == c) max else curr + 1

    def longest(l1: List[Input], l2: List[Input]): List[Input]= 
      if (l1.length >= l2.length) l1 else l2

    def shortest(l1: List[Input], l2: List[Input]): List[Input]= 
      if (l1.length >= l2.length) l2 else l1

    def calc (l: List[Input])(selector: (List[Input],List[Input]) => List[Input]): List[Input] = {
      val maxPosition = l(0).length
      val incrementor = incrementOrMax(maxPosition)(_: Int)(_: Int,_: Int)

      def innerCalc(subList: List[Input], position: Int): List[Input] = {
        return if(position >= maxPosition) subList else {
          val ones = subList.filter(isOn(position))
          val zeroes = subList.filter(isOff(position))

          innerCalc(selector(ones,zeroes), incrementor(position, ones.length, zeroes.length))
        }
      }
      return innerCalc(l, 0)
    }


    val o2 = fromBinToInt(calc(list)(longest).head)
    val co2 = fromBinToInt(calc(list)(shortest).head)

    return o2 * co2
  }

  def addIt(c: Count) (s: Char) = s match {
    case '1' => Count(c.z, c.o + 1)
    case '0' => Count(c.z + 1, c.o)
  }
}
