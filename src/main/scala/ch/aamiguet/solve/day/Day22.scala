package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import ch.aamiguet.util.readLines

object Day22 extends Day {
  val dayId = 22

  case class AxisBoundary(val min: Int, val max: Int) {
    def initializationRegion: Boolean = min >= -50 && max <= 50
  }

  object AxisBoundary {

    def apply(str: String): AxisBoundary = {
      val sp = str.split("\\.\\.")
      AxisBoundary(sp(0).toInt, sp(1).toInt)
    }

  }

  case class Step(state: Boolean, xb: AxisBoundary, yb: AxisBoundary, zb: AxisBoundary) {
    def initializationRegion: Boolean = xb.initializationRegion && yb.initializationRegion && zb.initializationRegion
  }

  object Step {

    def apply(str: String): Step = {
      val sp = str.split(" ")
      val boundaries = sp(1).split(",")
      Step(
        sp(0) == "on",
        AxisBoundary(boundaries(0).drop(2)),
        AxisBoundary(boundaries(1).drop(2)),
        AxisBoundary(boundaries(2).drop(2)),
      )
    }

  }

  case class Cube(val x: Int, val y: Int, val z: Int)

  def parseSteps(lines: List[String]) = lines.map(Step.apply)

  def naiveReboot(steps: List[Step]): Set[Cube] = {
    def updateCubes(step: Step, litCubes: Set[Cube]) = {
      val cubes =
        (for {
          x <- step.xb.min to step.xb.max
          y <- step.yb.min to step.yb.max
          z <- step.zb.min to step.zb.max
        } yield Cube(x, y, z)).toSet
      if (step.state)
        litCubes ++ cubes
      else
        litCubes -- cubes
    }

    def loop(steps: List[Step], litCubes: Set[Cube]): Set[Cube] =
      if (steps.isEmpty)
        litCubes
      else
        loop(steps.tail, updateCubes(steps.head, litCubes))

    loop(steps, Set.empty[Cube])
  }

  lazy val lines = readLines(filename)
  lazy val steps = parseSteps(lines)

  def part1 = {
    val cubes = naiveReboot(steps.filter(_.initializationRegion))
    println(s"The number of lit cubes is ${cubes.size}")
  }
  def part2 = ???
}
