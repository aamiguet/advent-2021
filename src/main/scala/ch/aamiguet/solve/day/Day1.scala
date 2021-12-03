package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.annotation.tailrec

import scala.io.Source

case object Day1 extends Day {

  @tailrec
  def countIncreasing(ms: List[Int], currentDepth: Option[Int], acc: Int): Int =
    ms match {
      case Nil =>
        acc
      case head :: tail =>
        val newAcc =
          currentDepth.fold(0)(c =>
            if (c < head)
              acc + 1
            else
              acc
          )
        countIncreasing(tail, Some(head), newAcc)
    }

  def countIncreasing(ms: List[Int]): Int = countIncreasing(ms, None, 0)

  def slidingWindow(ms: List[Int]): List[Int] =
    ms.lazyZip(ms.tail).lazyZip(ms.tail.tail).toList.map((a, b, c) => a + b + c)

  def readFile(filename: String): List[Int] = Source.fromFile(filename).getLines.toList.map(_.toInt)

  def part1 = {
    val filename = s"ressources/day1.txt"
    val ms = readFile(filename)
    println(s"Part I : the number of increasing depth measurement is ${countIncreasing(ms)}")
  }

  def part2 = {
    val filename = s"ressources/day1.txt"
    val ms = readFile(filename)
    println(s"Part II : the number of increasing depth measurement with sliding windows is ${countIncreasing(slidingWindow(ms))}")
  }

}
