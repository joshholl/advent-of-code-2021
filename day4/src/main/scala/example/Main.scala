package example

import scala.collection.mutable.ListBuffer
import scala.io.Source._

class GameBoard(val boardNum: Int, values: List[Int]) {

  val marked = ListBuffer[Int]()
  val bounds = Array((0, 4), (5, 9), (10, 14), (15, 19), (20, 24))
  
  def didWin(): Boolean = {
    if (marked.length < 5) { return false }
    else {
      val colWin = marked.map(m => m % 5).groupBy(identity).exists {
        case (value, items) => items.length == 5
      }
      val rowWin = bounds.exists { case (lower, upper) =>
        marked.filter(i => i <= upper && i >= lower).length == 5
      }
      return colWin || rowWin
    }
  }

  def mark(value: Int): GameBoard = {
    val index = values.indexOf(value)
    if(index >= 0) {
      marked.addOne(index)
    }
    this
  }

  def scoreCard(winningNumber: Int) = 
    values.zipWithIndex.foldLeft(0) { (acc, valuePair) =>
      {
        val (value, index) = valuePair
        if (!marked.contains(index)) {
          acc + value
        } else {
          acc
        }
      }
    } * winningNumber
  

  override def toString(): String = s"Board $boardNum, marked: $marked"
  override def clone(): GameBoard = new GameBoard(boardNum, values)
}

object Day4 {
  val testLines =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"""

  def main(args: Array[String]) = {
    val input =
      (if (args.isEmpty) testLines.split("\n").toList
       else fromFile(args(0)).getLines.toList)
    val callOuts = input.head.split(",").map(_.toInt)

    var boardNum = 0
    val boards = input.tail
      .filter(!_.isBlank())
      .grouped(5)
      .map(_.flatMap(_.split(" ").filter(!_.isBlank()).map(_.toInt)))
      .zipWithIndex
      .map { case (numbers, index) => new GameBoard(index + 1, numbers) }
      .toList

    println(s"playing game with ${boards.length}")

    val part1Answer = part1(boards, callOuts)
    val part2Answer = part2(boards.map(_.clone), callOuts)

    println(s"Part 1 = $part1Answer")
    println(s"Part 2 = $part2Answer")
  }

  def part1(boards: List[GameBoard], numbers: Array[Int]): Int = {
    if (numbers.length == 0) {return 0}

    val number = numbers.head
    // println(s"Calling $number")
    val start: (List[GameBoard], List[GameBoard]) = (List.empty, List.empty)
    val (winners, losers) = boards.map(_.mark(number)).foldLeft(start) {
      case (
            (accWinners: List[GameBoard], accLosers: List[GameBoard]),
            board: GameBoard
          ) =>
        if (board.didWin) (accWinners.appended(board), accLosers)
        else (accWinners, accLosers.appended(board))
    }

    return if (winners.length > 0) {
      winners.head.scoreCard(number)
    } else {
      part1(losers, numbers.tail)
    }
  }

  def part2(boards: List[GameBoard], numbers: Array[Int]): Int = {

      if(numbers.length==0 ) {
        return 0
      } else {
        val currentNumber = numbers.head
        val start: (List[GameBoard], List[GameBoard]) = (List.empty, List.empty)
        val (winners, losers) = boards.map(_.mark(currentNumber)).foldLeft(start) { case((accWinners, accLosers), board) => {
          if(board.didWin) (accWinners.appended(board),accLosers) else (accWinners, accLosers.appended(board))
        }}
        return if(losers.isEmpty) {
          val winnerCard = winners.last
          winners.last.scoreCard(currentNumber)
        } else {
          part2(losers, numbers.tail)
        }

      }
  }
}
