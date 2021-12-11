package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day7Spec extends Specification {

  val crabs = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

  "Day 7 specification".br

  "Fuel cost" >> {
    Day7.fuelCost(crabs, 2) mustEqual 37
  }

  "Optimal position" >> {
    Day7.optimalPos(crabs) mustEqual (2, 37)
  }


}
