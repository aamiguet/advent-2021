package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day1Spec extends Specification {

  val measurements = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
  val slidingWindowsMeasurements = List(607, 618, 618, 617, 647, 716, 769, 792)

  "Day 1 specification".br

  "The increasing count should be 7" >> {
    Day1.countIncreasing(measurements) mustEqual 7
  }

  "Transforming the measurements to a sliding window" >> {
    val swms = Day1.slidingWindow(measurements)
    swms mustEqual slidingWindowsMeasurements
  }

  "The increasing count should be 5" >> {
    Day1.countIncreasing(Day1.slidingWindow(measurements)) mustEqual 5
  }

}
