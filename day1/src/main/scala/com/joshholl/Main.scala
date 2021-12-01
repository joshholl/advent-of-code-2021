import scala.io.Source._


object Main {
  def main(args: Array[String]): Unit = {

    val lines = fromFile(args(0)).getLines


    val intLines = lines.map(_.toInt).toList

    val slidingLines = intLines.sliding(3).toList.map(_.reduce(_ + _))

    print("Single Lines: ")
    println(sumIt(intLines))

    print("Slidng Lines: ")
    println(sumIt(slidingLines))


  }

  def sumIt(theList: List[Int]): Int = {

    val initial: (Option[Int], Int)  = (None, 0)
    return theList.foldLeft(initial)((a, c) => {
      a match {
        case x if x._1.isDefined && x._1.get < c => (Some(c), x._2 + 1)
        case _ => (Some(c), a._2)
      }
    })._2
  }
}



