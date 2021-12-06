package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day3Spec extends Specification {

  val lines = Day3.toInt(
    List(
      "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010",
    )
  )

  val (gamma, epsilon) = (22, 9)
  val oxygen = 23
  val co2 = 10

  "Day 3 specification".br

  "Computing gamma & epsilon rates for test lines" >> {
    Day3.rates(lines) mustEqual (gamma, epsilon)
  }

  "Oyxgen rate for test lines" >> {
    Day3.oygenRating(lines) mustEqual oxygen
  }

  "CO2 scrubber rate for test lines" >> {
    Day3.co2Rating(lines) mustEqual co2
  }

}
