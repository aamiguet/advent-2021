package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day7Spec extends Specification {

  val crabs = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

  "Day 7 specification".br

  "Fuel cost" >> {
    Day7.fuelCost(crabs, 2)(Day7.linearFuelCost) mustEqual 37
  }

  "Optimal position with linear fuel cost" >> {
    Day7.optimalPos(crabs)(Day7.linearFuelCost) mustEqual (2, 37)
  }

  "Optimal position with increasing fuel cost" >> {
    Day7.optimalPos(crabs)(Day7.increasingFuelCost) mustEqual (5, 168)
  }

}
