package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.io.Source
import java.awt.geom.Point2D

case class Point(val x: Int, val y: Int)

object Point {

  def apply(coords: String): Point = {
    val cs = coords.split(",")
    Point(cs(0).toInt, cs(1).toInt)
  }

}

case class Vent(start: Point, end: Point) {
  lazy val isVertical: Boolean = start.x == end.x
  lazy val isHorizontal: Boolean = start.y == end.y

  lazy val stepY =
    if (end.y > start.y)
      1
    else
      -1

  lazy val rangeY = start.y to end.y by stepY

  lazy val stepX =
    if (end.x > start.x)
      1
    else
      -1

  lazy val rangeX = start.x to end.x by stepX

  lazy val allPoints: Set[Point] = {
    if (isVertical) {
      rangeY.map(y => Point(start.x, y)).toSet
    } else if (isHorizontal) {
      rangeX.map(x => Point(x, start.y)).toSet
    } else {
      val zipped = rangeX.zip(rangeY)
      zipped.map((x, y) => Point(x, y)).toSet
    }
  }

}

object Vent {

  def apply(ventPoints: String): Vent = {
    val vps = ventPoints.split(" -> ")
    Vent(Point(vps(0)), Point(vps(1)))
  }

}

object Day5 extends Day {
  val dayId = 5

  lazy val vents = Source.fromFile(filename).getLines.toList.map(Vent.apply)

  def ventPointMap(
    vents: List[Vent]
  ): Map[Point, List[Point]] = {
    val allPoints = vents.flatMap(_.allPoints)
    allPoints.groupBy(p => p)
  }

  def dangerousPoints(m: Map[Point, List[Point]]): List[Point] =
    m.filter(_._2.length >= 2).keys.toList

  def dangerousPointsCount(m: Map[Point, List[Point]]): Int = dangerousPoints(m).length

  def horizontalVerticalDangerousPointCount(
    vents: List[Vent]
  ): Int = {
    val filteredVents = vents.filter(v => v.isHorizontal || v.isVertical)
    dangerousPointsCount(ventPointMap(filteredVents))
  }

  def allDangerousPointCount(
    vents: List[Vent]
  ): Int = dangerousPointsCount(ventPointMap(vents))

  def part1 = {
    val c = horizontalVerticalDangerousPointCount(vents)
    println(s"There are $c dangerous points taking horizontal and vertical vents into account")
  }

  def part2 = {
    val c = allDangerousPointCount(vents)
    println(s"There are $c dangerous points in total")
  }

}
