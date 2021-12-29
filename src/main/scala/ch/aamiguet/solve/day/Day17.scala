package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import ch.aamiguet.math.naturalSum

object Day17 extends Day {
  val dayId = 17

  val y1 = -108
  val y2 = -150
  val x1 = 81
  val x2 = 129

  def part1 = {
    val y0 = math.abs(y2) - 1
    val r =
      s"""Well this part is solvable analytically, x speed is meaningless
      |because we can always choose one such that the probe comes to a stop
      |on the x-axis; y-axis position after n steps is given by the formula
      |ny0 + (n-1)n/2. So we need to maximize y0 (initial speed) to get to
      |the highest position. As the trajectory is symetrical, its speed is
      |y0 + 1 once it gets back to the y = 0 position. So y0 must be less
      |than the bottom coordinate of the target area. So in our case that
      |is y0 = ${y0} and the highest point is then ${naturalSum(y0)}""".stripMargin
    println(r)
  }

  def part2 = ???

}
