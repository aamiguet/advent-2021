package ch.aamiguet.solve.day

import org.specs2.mutable.Spec
import org.specs2.mutable.Specification

class Day11Spec extends Specification {
  "Day 11 Specification".br

  val smallGrid = OctopusGrid(
    List(
      "11111",
      "19991",
      "19191",
      "19991",
      "11111",
    )
  )

  val grid = OctopusGrid(
    List(
      "5483143223",
      "2745854711",
      "5264556173",
      "6141336146",
      "6357385478",
      "4167524645",
      "2176841721",
      "6882881134",
      "4846848554",
      "5283751526",
    )
  )

  "Small grid, after one step" >> {
    Day11.flashingCount(smallGrid, 1) mustEqual 9
  }

  "Total flashing count" >> {
    Day11.flashingCount(grid, 100) mustEqual 1656
  }

  "Synchronizing step" >> {
    grid.synchroStep() mustEqual 195
  }

}
