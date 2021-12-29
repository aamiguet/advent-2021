package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

import Day17.*

class Day17Spec extends Specification {
  "Day 17 Specification".br

  "Min x speed" >> {
    minXSpeed(20) mustEqual 6
  }

  "X Position" >> {
    xPosition(1, 1) mustEqual 1
    xPosition(5, 2) mustEqual 9
    xPosition(25, 2) mustEqual 49
    xPosition(3, 15) mustEqual 6
  }

  "Max step" >> {
    maxStep(25, 30) mustEqual Some(1)
  }

  "Min step" >> {
    minStep(25, 20) mustEqual 1
    minStep(13, 20) mustEqual 2
    minStep(20, 20) mustEqual 1
  }

  "Finds target" >> {
    findsTarget(-2, 1, Some(1), -5, -10) mustEqual false
    findsTarget(-10, 1, Some(1), -5, -10) mustEqual true
  }

  "Finding all velocities" >> {
    val vs = velocities(20, 30, -5, -10)
    vs.size mustEqual 112
  }

}
