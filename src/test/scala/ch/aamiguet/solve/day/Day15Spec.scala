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
    positions.length mustEqual 100
  }

  "Destination risk" >> {
    destinationRisk(lines) mustEqual 40
  }

  "Tile risk" >> {
    tileRisk(9, 0, 0) mustEqual 9
    tileRisk(5, 1, 0) mustEqual 6
    tileRisk(6, 2, 1) mustEqual 9
    tileRisk(4, 5, 5) mustEqual 5
  }

  "Full destination risk" >> {
    fullDestinationRisk(lines) mustEqual 315
  }

}
