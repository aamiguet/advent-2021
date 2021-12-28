package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

import Day15.*

class Day15Spec extends Specification {
  "Day 15 Specification".br

  lazy val lines: List[String] = List(
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581",
  )

  lazy val positions: List[Position] = Position.parsePositions(lines)

  "Parsing position" >> {
    positions.length mustEqual 99
  }

  "Destination risk" >> {
    destinationRisk(lines) mustEqual 40
  }

}
