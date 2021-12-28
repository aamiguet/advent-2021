package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day

object Day15 extends Day {

  case class Position(
    val x: Int,
    val y: Int,
    val risk: Int,
    val cumulativeRisk: Int = Int.MaxValue,
  ) {
    def withCumulativeRisk(c: Int): Position = Position(x, y, risk, c + risk)
  }

  object Position {

    def parsePositions(lines: List[String]): List[Position] =
      val arr = lines.toArray
      (for {
        x <- 0 until arr.length
        y <- 0 until arr(x).length if x > 0 || y > 0 // skip top-left
      } yield Position(x, y, arr(x)(y).asDigit)).toList

  }

  def isNeighbor(p: Position)(origin: Position): Boolean = {
    val xAxisNeighbor = p.x == origin.x && Math.abs(p.y - origin.y) == 1
    val yAxisNeighbor = p.y == origin.y && Math.abs(p.x - origin.x) == 1
    xAxisNeighbor || yAxisNeighbor
  }

  def discover(visited: List[Position], unvisited: List[Position]): List[Position] =
    if (unvisited.size == 0) visited
    else {
      val sorted = unvisited.sortBy(_.cumulativeRisk)
      val current = sorted.head
      val (neighbors, others) = sorted.tail.partition(isNeighbor(current))
      val updatedNs = neighbors.map { n =>
        val updatedN = n.withCumulativeRisk(current.cumulativeRisk)
        if (updatedN.cumulativeRisk < n.cumulativeRisk)
          updatedN
        else
          n
      }
      discover(current :: visited, updatedNs ++ others)
    }

  def discoverFromOrigin(positions: List[Position]): List[Position] =
    val origin = Position(0, 0, 0, 0)
    discover(Nil, origin :: positions)

  def destinationRisk(positions: List[Position], x: Int, y: Int): Int =
    positions.filter(p => p.x == x && p.y == y).head.cumulativeRisk

  def destinationRisk(lines: List[String]): Int = {
    val positions = discoverFromOrigin(Position.parsePositions(lines))
    val x = lines.length - 1
    val y = lines.head.length - 1
    destinationRisk(positions, x, y)
  }

  val dayId = 15

  lazy val lines =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList

  def part1 = println(s"The minimal destination risk is ${destinationRisk(lines)}")
  def part2 = ???
}
