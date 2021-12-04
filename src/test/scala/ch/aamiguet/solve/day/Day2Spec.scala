package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day2Spec extends Specification {

  val lines = List(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2",
  )

  val moves = List(
    Move(Direction.Forward, 5),
    Move(Direction.Down, 5),
    Move(Direction.Forward, 8),
    Move(Direction.Up, 3),
    Move(Direction.Down, 8),
    Move(Direction.Forward, 2),
  )

  val originalPos = Position(0, 0)
  val finalPos = Position(15, 10)
  val finalPosWithAim = Position(15, 60, 10)

  "Day 2 specification".br

  "Parsing lines should gives us the moves" >> {
    Day2.parseMoves(lines) mustEqual moves
  }

  "The final position is (15, 10)" >> {
    Day2.finalPosition(originalPos, moves) mustEqual finalPos
  }

  "The final position with aim (15, 60)" >> {
    Day2.finalPositionWithAim(originalPos, moves) mustEqual finalPosWithAim
  }

  "The final position product is 150" >> {
    finalPos.product mustEqual 150
  }

}
