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

  object DiractDice {
    val MOVES = Map(
      (3 -> 1L),
      (4 -> 3L),
      (5 -> 6L),
      (6 -> 7L),
      (7 -> 6L),
      (8 -> 3L),
      (9 -> 1L),
    )
  }

  trait AbstractPlayer {
    def position: Int
    def score: Int
    def move(s: Int): AbstractPlayer

    def newPos(s: Int) = modUp(position + s, 10)
  }

  case class Player(val position: Int, val score: Int) extends AbstractPlayer {

    def move(s: Int): Player = {
      val np = newPos(s)
      Player(np, score + np)
    }

  }

  object Player {
    def apply(line: String): Player = Player(s"${line.last}".toInt, 0)
  }

  case class Player1(val position: Int, val score: Int) extends AbstractPlayer {

    def move(s: Int): Player1 = {
      val np = newPos(s)
      Player1(np, score + np)
    }

  }

  object Player1 {
    def apply(line: String): Player1 = Player1(s"${line.last}".toInt, 0)
  }

  case class Player2(val position: Int, val score: Int) extends AbstractPlayer {

    def move(s: Int): Player2 = {
      val np = newPos(s)
      Player2(np, score + np)
    }

  }

  object Player2 {
    def apply(line: String): Player2 = Player2(s"${line.last}".toInt, 0)
  }

  trait Universe {
    def nextStates(moves: Map[Int, Long]): Map[Universe, Long]
  }

  case class GameState(val player: AbstractPlayer, val opponent: AbstractPlayer) extends Universe {

    private def winningUniverse(player: AbstractPlayer): Universe =
      player match {
        case Player1(_, _) =>
          Player1Win
        case Player2(_, _) =>
          Player2Win
        case _ =>
          throw new Exception("Undefined")
      }

    def nextStates(moves: Map[Int, Long]): Map[Universe, Long] =
      moves.foldLeft(Map.empty[Universe, Long]){
        (acc, m) => {
          val newPlayer = player.move(m._1)
          val gs =
            if (newPlayer.score >= 21) {
              winningUniverse(newPlayer)
            } else {
              GameState(opponent, newPlayer)
            }
          acc.updatedWith(gs){
            case Some(l) => Some(l + m._2)
            case None => Some(m._2)
          }
        }
    }

  }

  case object Player1Win extends Universe {
    def nextStates(moves: Map[Int, Long]) = Map(this -> 1L)
  }

  case object Player2Win extends Universe {
    def nextStates(moves: Map[Int, Long]) = Map(this -> 1L)
  }

  def play(player: Player, opponent: Player, die: Die): (Player, Player, Die) =
    if (opponent.score >= 1000)
      (opponent, player, die)
    else
      play(opponent, player.move(die.value), die.next)

  def play(gss: Map[Universe, Long]): Map[Universe, Long] =
    if (gss.keySet.forall(gs => gs == Player1Win || gs == Player2Win))
      gss
    else {
      val newGss = gss.foldLeft(Map.empty[Universe, Long]) { (acc, gs) =>
        val ns = gs._1.nextStates(DiractDice.MOVES)
        ns.foldLeft(acc) { (innerAcc, n) =>
          innerAcc.updatedWith(n._1) {
            case Some(l) =>
              Some(l + gs._2 * n._2)
            case None =>
              Some(gs._2 * n._2)
          }
        }
      }
      play(newGss)
    }

  def play(p1: Player1, p2: Player2): Map[Universe, Long] =
    val m: Map[Universe, Long] = Map(GameState(p1, p2) -> 1L)
    play(m)

  def finalScore(gss: Map[Universe, Long]): Long = {
    math.max(gss.getOrElse(Player1Win, 0L), gss.getOrElse(Player2Win, 0L))
  }

  def finalScore(winner: Player, loser: Player, die: Die): Int = die.rolls * loser.score

  lazy val lines = scala
    .io
    .Source
    .fromFile(filename)
    .getLines
    .toList

  lazy val players = lines
    .map(Player.apply)

  lazy val (player1, player2) = (Player1(lines.head), Player2(lines.last))

  def part1 = {
    val dd = DeterministicDie(100, 0, 0)
    val (w, l, d) = play(players.head, players.last, dd)
    println(s"The final score is ${finalScore(w, l, d)}")
  }

  def part2 = {
    val gss = play(player1, player2)
    println(s"The final score of the Dirac Dice game is ${finalScore(gss)}")
  }

}
