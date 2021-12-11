package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.io.Source
import scala.annotation.tailrec

object Day6 extends Day {
  val dayId = 6

  lazy val fishes = Source
    .fromFile(filename)
    .getLines
    .toList
    .flatMap(line => line.split(",").map(_.toInt))

  def asMap(fishes: List[Int]) = fishes.groupBy(f => f).mapValues(v => BigInt(v.length)).toMap

  lazy val fishesMap = asMap(fishes)

  def nextFishes(fishes: Map[Int, BigInt]): Map[Int, BigInt] =
    fishes.foldLeft(Map.empty[Int, BigInt]) { (acc, f) =>
      val key = f._1
      val number = f._2
      if (key == 0) {
        acc.updatedWith(6) {
          case Some(n) =>
            Some(n + number)
          case None =>
            Some(number)
        } + (8 -> number)
      } else {
        val newKey = key - 1
        acc.updatedWith(newKey) {
          case Some(n) =>
            Some(n + number)
          case None =>
            Some(number)
        }
      }
    }

  @tailrec
  def fishesCount(fishes: Map[Int, BigInt], days: Int): BigInt =
    if (days == 0)
      fishes.values.sum
    else
      fishesCount(nextFishes(fishes), days - 1)

  def part1 = println(s"Count after 80 days is ${fishesCount(fishesMap, 80)}")
  def part2 = println(s"Count after 256 days is ${fishesCount(fishesMap, 256)}")
}
