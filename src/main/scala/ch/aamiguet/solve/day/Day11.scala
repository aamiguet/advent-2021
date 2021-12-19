package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import cats.instances.tailRec
import scala.annotation.tailrec

type Position = (Int, Int)

case class Octopus(val level: Int, val isFlashing: Boolean = false) {
  val readyToFlash = level > 9 && !isFlashing

  val prettyString: String = {
    if (isFlashing)
      "0"
    else
      level.toString
  }

}

object Octopus {
  def apply(level: String): Octopus = Octopus(level.toInt)
}

case class OctopusGrid(val octopuses: Map[Position, Octopus]) {

  def increaseEnergy: OctopusGrid = OctopusGrid(
    octopuses.mapValues(o => Octopus(o.level + 1)).toMap
  )

  def adjacentPositions(position: Position): List[Position] = List(
    (position._1 - 1, position._2 - 1),
    (position._1 - 1, position._2),
    (position._1 - 1, position._2 + 1),
    (position._1, position._2 - 1),
    (position._1, position._2 + 1),
    (position._1 + 1, position._2 - 1),
    (position._1 + 1, position._2),
    (position._1 + 1, position._2 + 1),
  )

  def udpatedWithEnergyIncrease(os: Map[Position, Octopus], pos: Position) =
    os.updatedWith(pos) {
      case None =>
        None
      case Some(o) =>
        Some(Octopus(o.level + 1, o.isFlashing))
    }

  def updatedWithFlashing(os: Map[Position, Octopus], pos: Position) =
    os.updatedWith(pos) {
      case None =>
        None
      case Some(o) =>
        Some(Octopus(o.level, true))
    }

  def increaseEnergy(positions: List[Position]): OctopusGrid =
    if (positions.isEmpty) OctopusGrid(octopuses)
    else {
      val adjacents = adjacentPositions(positions.head)
      val newOctopuses = adjacents.foldLeft(octopuses)(udpatedWithEnergyIncrease)
      val newGrid = OctopusGrid(newOctopuses)
      newGrid.increaseEnergy(positions.tail)
    }

  def updateFlashing(positions: List[Position]): OctopusGrid = {
    val newOctopuses = positions.foldLeft(octopuses)(updatedWithFlashing)
    OctopusGrid(newOctopuses)
  }

  def flashingPos: List[Position] = octopuses.filter(_._2.readyToFlash).keys.toList

  def flashingCount: Int = octopuses.filter(_._2.isFlashing).size

  def nextStep: OctopusGrid = OctopusGrid(
    octopuses
      .mapValues(o =>
        if (o.isFlashing)
          Octopus(0)
        else
          Octopus(o.level)
      )
      .toMap
  )

  def resolveFlashing: OctopusGrid = {
    val pos = flashingPos
    if (pos.isEmpty)
      this
    else
      updateFlashing(pos).increaseEnergy(pos).resolveFlashing
  }

  def prettyPrint: String = {
    val size = Math.sqrt(octopuses.size).toInt
    val range = (0 until size).toList
    val content =
      range.map(i =>
        range.map(j => octopuses.get((i, j)).get.prettyString) mkString ""
      ) mkString "\n"
    s"""|-----
    |$content
    |-----""".stripMargin
  }

  def synchroStep(currentStep: Int = 1): Int = {
    val computedGrid = increaseEnergy.resolveFlashing
    if (computedGrid.octopuses.values.forall(_.isFlashing))
      currentStep
    else
      computedGrid.nextStep.synchroStep(currentStep + 1)
  }

}

object OctopusGrid {

  def apply(lines: List[String]): OctopusGrid = {
    val octopuses =
      for {
        i <- 0 until lines.size
        j <- 0 until lines(0).size
      } yield (i, j) -> Octopus(lines(i).split("")(j))
    OctopusGrid(octopuses.toMap)
  }

}

object Day11 extends Day {
  val dayId = 11

  lazy val lines =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList

  lazy val grid = OctopusGrid(lines)

  @tailrec
  def computeRounds(grid: OctopusGrid, rounds: Int, flashes: List[Int]): List[Int] =
    if (rounds == 0) flashes
    else {
      val computedGrid = grid.increaseEnergy.resolveFlashing
      computeRounds(computedGrid.nextStep, rounds - 1, computedGrid.flashingCount :: flashes)
    }

  def flashingCount(grid: OctopusGrid, rounds: Int): Int =
    computeRounds(grid, rounds, List.empty[Int]).sum

  def part1 = println(s"Flashing count after 100 rounds is ${flashingCount(grid, 100)}")

  def part2 = println(
    s"We have to wait ${grid.synchroStep()} steps until all octopi are synchronized"
  )

}
