package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day4Spec extends Specification {

  val bingo =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      |8  2 23  4 24
      |21  9 14 16  7
      |6 10  3 18  5
      |1 12 20 15 19
      |
      |3 15  0  2 22
      |9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      |2  0 12  3  7""".stripMargin.split("\n").toList

  val bs = BingoSubsystem(bingo)

  val winningBoard = BingoBoard(
    List(
      List(
        BingoNumber(14, true),
        BingoNumber(21, true),
        BingoNumber(17, true),
        BingoNumber(24, true),
        BingoNumber(4, true),
      ),
      List(
        BingoNumber(10, false),
        BingoNumber(16, false),
        BingoNumber(15, false),
        BingoNumber(9, true),
        BingoNumber(19, false),
      ),
      List(
        BingoNumber(18, false),
        BingoNumber(8, false),
        BingoNumber(23, true),
        BingoNumber(26, false),
        BingoNumber(20, false),
      ),
      List(
        BingoNumber(22, false),
        BingoNumber(11, true),
        BingoNumber(13, false),
        BingoNumber(6, false),
        BingoNumber(5, true),
      ),
      List(
        BingoNumber(2, true),
        BingoNumber(0, true),
        BingoNumber(12, false),
        BingoNumber(3, false),
        BingoNumber(7, true),
      ),
    )
  )

  val winningScore = 4512

  "Day 4 specification".br

  "Winning board should be winning" >> {
    winningBoard.isWinning mustEqual true
  }

  s"Sum of the unchecked number should be 188" >> {
    winningBoard.uncheckedNumbersSum mustEqual 188
  }

  s"Final score shoud be $winningScore" >> {
    Day4.finalScore(bs) mustEqual winningScore
  }

  s"Ultimate score shoud be 1924" >> {
    Day4.ultimateScore(bs) mustEqual 1924
  }
}
