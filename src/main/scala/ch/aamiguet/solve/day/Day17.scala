package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import ch.aamiguet.math.naturalSum

object Day17 extends Day {
  val dayId = 17

  val y1 = -108
  val y2 = -150
  val x1 = 81
  val x2 = 129

  def xPosition(x0: Int, n: Int): Int =
    if (n >= x0)
      naturalSum(x0)
    else {
      n * x0 - naturalSum(n - 1)
    }

  def yPosition(y0: Int, n: Int): Int = n * y0 - naturalSum(n - 1)

  def minStep(x0: Int, x1: Int): Int = {
    def loop(n: Int): Int =
      if (xPosition(x0, n) >= x1)
        n
      else
        loop(n + 1)
    loop(1)
  }

  def maxStep(x0: Int, x2: Int): Option[Int] =
    def loop(n: Int): Int =
      if (xPosition(x0, n) > x2)
        n - 1
      else
        loop(n + 1)
    if (naturalSum(x0) <= x2)
      None
    else
      Some(loop(1))

  def maxYSpeed(y: Int) = math.abs(y) - 1

  def minXSpeed(x: Int) = {
    def loop(x0: Int): Int =
      if (naturalSum(x0) >= x)
        x0
      else
        loop(x0 + 1)
    loop(1)
  }

  def findsTarget(y0: Int, n0: Int, n1: Option[Int], y1: Int, y2: Int): Boolean = {
    def loop(n: Int): Boolean = {
      val pos = yPosition(y0, n)
      if (pos < y2)
        false
      else if (n1.isDefined && n > n1.get)
        false
      else if (pos <= y1 && pos >= y2)
        true
      else
        loop(n + 1)
    }
    loop(n0)
  }

  def velocities(x1: Int, x2: Int, y1: Int, y2: Int): Set[(Int, Int)] = {
    val minX0 = minXSpeed(x1)
    val maxX0 = x2
    val minY0 = y2
    val maxY0 = maxYSpeed(y2)
    (for {
      x0 <- minX0 to maxX0
      n0 <- List(minStep(x0, x1))
      n1 <- List(maxStep(x0, x2))
      y0 <- minY0 to maxY0 if findsTarget(y0, n0, n1, y1, y2)
    } yield (x0, y0)).toSet
  }

  def part1 = {
    val y0 = maxYSpeed(y2)
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

  def part2 = {
    val vs = velocities(x1, x2, y1, y2)
    println(s"There are ${vs.size} distinct velocity values possible")
  }

}
