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
        y <- 0 until arr(x).length
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
    val ps = positions.filter(p => p.x > 0 || p.y > 0)
    discover(Nil, origin :: ps)

  def destinationRisk(positions: List[Position], x: Int, y: Int): Int =
    positions.filter(p => p.x == x && p.y == y).head.cumulativeRisk

  def destinationRisk(lines: List[String]): Int = {
    val positions = discoverFromOrigin(Position.parsePositions(lines))
    val x = positions.map(_.x).max
    val y = positions.map(_.y).max
    destinationRisk(positions, x, y)
  }

  def tileRisk(risk: Int, x: Int, y: Int) =
    val r = (risk + x + y) % 9
    if (r == 0) 9
    else r

  def fullPositions(positions: List[Position]): List[Position] =
    val xSize = positions.map(_.x).max + 1
    val ySize = positions.map(_.y).max + 1
    val repeat = 5
    for {
      p <- positions
      x <- 0 until repeat
      y <- 0 until repeat
    } yield Position(p.x + x * xSize, p.y + y * ySize, tileRisk(p.risk, x, y))

  def fullDestinationRisk(lines: List[String]): Int = {
    val ps = fullPositions(Position.parsePositions(lines))
    val positions = discoverFromOrigin(ps)
    val x = positions.map(_.x).max
    val y = positions.map(_.y).max
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
  def part2 = println(s"This is taking time, but we know that the minimal full destination risk is ${fullDestinationRisk(lines)}")
}
