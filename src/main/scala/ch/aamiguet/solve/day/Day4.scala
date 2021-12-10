package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.io.Source

case class BingoNumber(val value: Int, val checked: Boolean) {

  def update(number: Int): BingoNumber =
    if (checked)
      this
    else
      BingoNumber(value, value == number)

}

object BingoNumber {

  def apply(number: String): BingoNumber = BingoNumber(number.trim.toInt, false)

}

case class BingoBoard(val lines: List[List[BingoNumber]]) {

  lazy val cols = lines.transpose

  def check(number: Int): BingoBoard = {
    val newLines = lines.map(_.map(n => n.update(number)))
    BingoBoard(newLines)
  }

  def winningLine(line: List[BingoNumber]): Boolean =
    line.foldLeft(true)((acc, n) => acc && n.checked)

  def winning(lines: List[List[BingoNumber]]): List[List[BingoNumber]] = lines.filter(winningLine)

  lazy val winningLines: List[List[BingoNumber]] = winning(lines)
  lazy val winningCols: List[List[BingoNumber]] = winning(cols)

  def isWinning = !winningLines.isEmpty || !winningCols.isEmpty

  def uncheckedNumbers: List[BingoNumber] = lines.flatten.filter(!_.checked)
  def uncheckedNumbersSum = this.uncheckedNumbers.map(_.value).sum

}

object BingoBoard {

  def fromString(lines: List[String]): BingoBoard = {
    val ls = lines.map(line => line.trim.split("\\s+").toList.map(BingoNumber.apply))
    BingoBoard(ls)
  }

}

case class BingoSubsystem(val boards: List[BingoBoard], val numbers: List[Int]) {

  lazy val newBoards = boards.map(_.check(numbers.head))

  lazy val winningBoards: List[BingoBoard] = newBoards.filter(_.isWinning)

  lazy val winningScore: Option[Int] = winningBoards
    .headOption
    .map { board =>
      board.uncheckedNumbersSum * numbers.head
    }

  def isWinning = !winningBoards.isEmpty

  def finalSubsystem: BingoSubsystem =
    if (this.isWinning)
      this
    else
      BingoSubsystem(newBoards, numbers.tail).finalSubsystem

  def ultimateSubsytem: BingoSubsystem =
    if (this.isWinning && newBoards.length == 1)
      this
    else
      BingoSubsystem(newBoards.filter(!winningBoards.contains(_)), numbers.tail).ultimateSubsytem

}

object BingoSubsystem {

  val bingoSize = 5

  def apply(input: List[String]): BingoSubsystem = {
    val numbers = input.head.split(",").toList.map(_.toInt)

    val grouped = input.tail.grouped(bingoSize + 1).toList
    val boards = grouped.map(lines => BingoBoard.fromString(lines.tail))

    BingoSubsystem(boards, numbers)
  }

}

object Day4 extends Day {
  val dayId = 4

  lazy val bs = BingoSubsystem(Source.fromFile(filename).getLines.toList)

  def finalScore(bs: BingoSubsystem): Int = bs.finalSubsystem.winningScore.head

  def ultimateScore(bs: BingoSubsystem): Int = bs.ultimateSubsytem.winningScore.head

  def part1 = println(s"Winning score is ${finalScore(bs)}")
  def part2 = println(s"Winning score of the last winning board is ${ultimateScore(bs)}")
}
