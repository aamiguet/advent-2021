package ch.aamiguet.solve.day

import org.specs2.mutable.Spec
import org.specs2.mutable.Specification

class Day9Spec extends Specification {

  "Day 8 Specification".br

  val heights: Array[Array[Int]] = Array(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678",
  ).map(l => l.split("").map(_.toInt))

  "Test data risk level" >> {
    Day9.riskLevel(Day9.lowPoints(heights)) mustEqual 15
  }

  "Product of the three largest basins" >> {
    Day9.productOfThreeLargestBasins(heights) mustEqual 1134
  }

}
