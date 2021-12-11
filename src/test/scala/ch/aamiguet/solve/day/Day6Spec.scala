package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day6Spec extends Specification {

  val fishes = Day6.asMap(List(3, 4, 3, 1, 2))

  "Day 6 specification".br

  "Count after 18 days" >> {
    Day6.fishesCount(fishes, 18) mustEqual BigInt(26)
  }

  "Count after 80 days" >> {
    Day6.fishesCount(fishes, 80) mustEqual BigInt(5934)
  }

  "Count after 256 days" >> {
    Day6.fishesCount(fishes, 256) mustEqual BigInt("26984457539")
  }

}
