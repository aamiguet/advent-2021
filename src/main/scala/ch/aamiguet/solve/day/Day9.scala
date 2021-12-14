package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import javax.xml.transform.Source

object Day9 extends Day {
  val dayId = 9

  lazy val heights = scala.io.Source
    .fromFile(filename)
    .getLines
    .toArray
    .map(l => l.split("").map(_.toInt))

  def isLower(heights: Array[Array[Int]], i: Int, j: Int): Boolean = {
    val current = heights(i)(j)
    val up = heights.lift(i-1).flatMap(_.lift(j))
    val left = heights.lift(i).flatMap(_.lift(j-1))
    val right = heights.lift(i).flatMap(_.lift(j+1))
    val down = heights.lift(i+1).flatMap(_.lift(j))

    up.fold(true)(current < _) && left.fold(true)(current < _) && right.fold(true)(current < _) && down.fold(true)(current < _)
  }

  def lowPoints(heights: Array[Array[Int]]): List[Int] =
    (for {
      i <- 0 until heights.length
      j <- 0 until heights(0).length if isLower(heights, i, j)
    } yield heights(i)(j)).toList

  def riskLevel(lowPoints: List[Int]): Int =
    lowPoints.map(_ + 1).sum

  def part1 = println(s"Risk level is ${riskLevel(lowPoints(heights))}")
  def part2 = ???

}
