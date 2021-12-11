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

  def nextFishes(fishes: List[Int]): List[Int] =
    fishes.foldLeft(List.empty[Int])((acc, fish) =>
      if (fish == 0)
        6 :: 8 :: acc
      else
        (fish - 1) :: acc
    )

  @tailrec
  def fishesCount(fishes: List[Int], days: Int): Int =
    if (days == 0)
      fishes.length
    else
      fishesCount(nextFishes(fishes), days - 1)

  def part1 = println(s"Count after 80 days is ${fishesCount(fishes, 80)}")
  def part2 = println(s"Count after 80 days is ${fishesCount(fishes, 256)}")
}
