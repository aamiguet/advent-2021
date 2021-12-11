package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day5Spec extends Specification {

  val vents = List(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2",
  ).map(Vent.apply)

  "Day 5 specification".br

  "Testing point parser" >> {
    Point("1,2") mustEqual Point(1, 2)
  }

  "Testing vent parser" >> {
    Vent("1,2 -> 3,4") mustEqual Vent(Point(1, 2), Point(3, 4))
  }

  "Testing all points" >> {
    Vent("1,1 -> 1,3").allPoints mustEqual Set(Point(1, 1), Point(1, 2), Point(1, 3))
  }

  "Dangerous points taking horizontal and vertical vents into account" >> {
    Day5.horizontalVerticalDangerousPointCount(vents) mustEqual 5
  }

  "Dangerous points taking all vents into account" >> {
    Day5.allDangerousPointCount(vents) mustEqual 12
  }

}
