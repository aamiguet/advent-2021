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

  def fuelCost(crabs: List[Int], pos: Int): Int = crabs.map(c => math.abs(c - pos)).sum

  def optimalPos(crabs: List[Int]): (Int, Int) = {
    val r = (crabs.min to crabs.max).toList
    val costs = r.map(i => (i, fuelCost(crabs, i)))
    costs.sortWith((a, b) => a._2 < b._2).head
  }

  def part1 = {
    val op = optimalPos(crabs)
    println(s"Optimal position is ${op._1} with a fuel consumption of ${op._2}")
  }

  def part2 = ???
}
