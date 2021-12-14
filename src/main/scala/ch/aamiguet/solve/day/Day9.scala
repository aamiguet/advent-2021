package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import javax.xml.transform.Source

object Day9 extends Day {
  val dayId = 9

  lazy val heights = scala
    .io
    .Source
    .fromFile(filename)
    .getLines
    .toArray
    .map(l => l.split("").map(_.toInt))

  def neighbors(heights: Array[Array[Int]], i: Int, j: Int): List[(Int, Int, Int)] = List(
    heights.lift(i - 1).flatMap(_.lift(j)).map(h => (i - 1, j, h)),
    heights.lift(i).flatMap(_.lift(j - 1)).map(h => (i, j - 1, h)),
    heights.lift(i).flatMap(_.lift(j + 1)).map(h => (i, j + 1, h)),
    heights.lift(i + 1).flatMap(_.lift(j)).map(h => (i + 1, j, h)),
  ).filter(_.isDefined).map(_.get)

  def isLower(heights: Array[Array[Int]], i: Int, j: Int): Boolean = {
    val current = heights(i)(j)
    val ns = neighbors(heights, i, j)

    ns.foldLeft(true)((acc, n) => acc && n._3 > current)
  }

  def lowPoints(heights: Array[Array[Int]]): List[(Int, Int, Int)] =
    (for {
      i <- 0 until heights.length
      j <- 0 until heights(0).length if isLower(heights, i, j)
    } yield (i, j, heights(i)(j))).toList

  def riskLevel(lowPoints: List[(Int, Int, Int)]): Int = lowPoints.map(_._3 + 1).sum

  def basinPoints(
    heights: Array[Array[Int]],
    point: (Int, Int, Int),
  ): Set[(Int, Int, Int)] = {
    val higherNs = neighbors(heights, point._1, point._2).filter(n => n._3 > point._3 && n._3 < 9)
    higherNs.foldLeft(Set(point))((acc, n) => acc ++ basinPoints(heights, n))
  }

  def basinsSize(heights: Array[Array[Int]], lowPoints: List[(Int, Int, Int)]): List[Int] =
    lowPoints.map(lp => basinPoints(heights, lp).size).sortWith((a, b) => a > b)

  def productOfThreeLargestBasins(heights: Array[Array[Int]]): Int = {
    val lp = lowPoints(heights)
    val bs = basinsSize(heights, lp)
    bs.head * bs.tail.head * bs.tail.tail.head
  }

  def part1 = println(s"Risk level is ${riskLevel(lowPoints(heights))}")

  def part2 = println(
    s"Product of the three largest basins is ${productOfThreeLargestBasins(heights)}"
  )

}
