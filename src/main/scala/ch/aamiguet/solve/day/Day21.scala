package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import ch.aamiguet.math.modUp

object Day21 extends Day {
  val dayId = 21

  trait Die {
    final val ROLLS = 3
    def value: Int
    def next: Die
    def rolls: Int
  }

  case class DeterministicDie(val sides: Int, val current: Int, val rolls: Int) extends Die {
    def value =
      (for {
        i <- 1 to ROLLS
      } yield modUp(current + i, sides)).toList.sum

    def next = {
      val newCurrent = modUp(current + 3, sides)
      DeterministicDie(sides, newCurrent, rolls + ROLLS)
    }

  }

  case class Player(val position: Int, val score: Int) {

    def move(s: Int): Player = {
      val newPos = modUp(position + s, 10)
      Player(newPos, score + newPos)
    }

  }

  object Player {
    def apply(line: String): Player = {
      Player(s"${line.last}".toInt, 0)
    }
  }

  def play(player: Player, opponent: Player, die: Die): (Player, Player, Die) =
    if (opponent.score >= 1000)
      (opponent, player, die)
    else
      play(opponent, player.move(die.value), die.next)

  def finalScore(winner: Player, loser: Player, die: Die): Int =
    die.rolls * loser.score

  lazy val players =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList
      .map(Player.apply)

  def part1 = {
    val dd = DeterministicDie(100, 0, 0)
    val (w, l, d) = play(players.head, players.last, dd)
    println(s"The final score is ${finalScore(w, l, d)}")
  }
  def part2 = ???

}
