package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

import Day21.*

class Day21Spec extends Specification {
  "Day 21 Specification".br

  val player1 = Player(4, 0)
  val player2 = Player(8, 0)
  val dd = DeterministicDie(100, 0, 0)

  "Final score" >> {
    val (w, l, d) = play(player1, player2, dd)
    finalScore(w, l, d) mustEqual 739785
  }

  "Final score of Dirac Dice" >> {
    val p1 = Player1(4, 0)
    val p2 = Player2(8, 0)
    val gss = play(p1, p2)
    finalScore(gss) mustEqual 444356092776315L
  }

}
