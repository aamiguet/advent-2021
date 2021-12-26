package ch.aamiguet.solve.day

import org.specs2.mutable.Specification
import Day13.*

class Day13Spec extends Specification {
  "Day 13 Specification".br

  val dots =
    List(
      "6,10",
      "0,14",
      "9,10",
      "0,3",
      "10,4",
      "4,11",
      "6,0",
      "6,12",
      "4,1",
      "0,13",
      "10,12",
      "3,4",
      "3,0",
      "8,4",
      "1,10",
      "2,14",
      "8,10",
      "9,0",
    ).map(Position.apply).toSet

  "Paper folding" >> {
    val d1 = foldPaper(dots, AlongY(7))
    d1.size mustEqual 17
    val d2 = foldPaper(d1, AlongX(5))
    printDots(d2)
    d2.size mustEqual 16
  }

  "Fold parsing" >> {
    Fold("fold along y=7") mustEqual AlongY(7)
    Fold("fold along x=5") mustEqual AlongX(5)
  }

}
