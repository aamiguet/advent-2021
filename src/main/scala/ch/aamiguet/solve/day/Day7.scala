package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.io.Source

object Day7 extends Day {
  val dayId = 7

  lazy val crabs = Source
    .fromFile(filename)
    .getLines
    .toList
    .flatMap(line => line.split(",").map(_.toInt))

  def fuelCost(crabs: List[Int], pos: Int)(fc: (Int, Int) => Int): Int =
    crabs.map(c => fc(c, pos)).sum

  def optimalPos(crabs: List[Int])(fc: (Int, Int) => Int): (Int, Int) = {
    val r = (crabs.min to crabs.max).toList
    val costs = r.map(i => (i, fuelCost(crabs, i)(fc)))
    costs.sortWith((a, b) => a._2 < b._2).head
  }

  def linearFuelCost(crabPos: Int, pos: Int) = math.abs(crabPos - pos)

  def increasingFuelCost(crabPos: Int, pos: Int) = {
    val diff = math.abs(crabPos - pos)
    (diff * (diff + 1)) / 2
  }

  def part1 = {
    val op = optimalPos(crabs)(linearFuelCost)
    println(s"Optimal position is ${op._1} with a linear fuel consumption of ${op._2}")
  }

  def part2 = {
    val op = optimalPos(crabs)(increasingFuelCost)
    println(s"Optimal position is ${op._1} with an increasing fuel consumption of ${op._2}")
  }

}
