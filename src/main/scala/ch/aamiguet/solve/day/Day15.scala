package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day

object Day15 extends Day {

  type Index = (Int, Int)

  case class Position(
    val risk: Int,
    val cumulativeRisk: Int = Int.MaxValue,
    val visited: Boolean = false,
  ) {
    def withCumulativeRisk(c: Int): Position = Position(risk, c + risk)
    def withVisited(b: Boolean): Position = Position(risk, cumulativeRisk, true)
  }

  object Position {

    def apply(risk: String): Position = Position(risk.toInt)

    def parsePositions(
      lines: List[String]
    ): Array[Array[Position]] = lines.toArray.map(line => line.split("").map(Position.apply))

  }

  def neighborIndexes(
    x: Int,
    y: Int,
  ): List[Index] = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))

  def discover(positions: Array[Array[Position]], toVisit: List[Index]): Unit =
    if (toVisit.isEmpty) {} else {
      // visitable position with lowest cumulative risk
      val sorted = toVisit.sortBy(p => positions(p._1)(p._2).cumulativeRisk)
      val (x, y) = sorted.head
      // marked as visited
      val currentPos = positions(x)(y).withVisited(true)
      positions(x).update(y, currentPos)
      // updating cumulative cost of neighbors
      val toUpdate = neighborIndexes(x, y).filter(i =>
        positions.isDefinedAt(i._1) && positions(i._1).isDefinedAt(i._2) && !positions(i._1)(
          i._2
        ).visited
      )
      toUpdate.map { i =>
        val neighbor = positions(i._1)(i._2)
        val updated = neighbor.withCumulativeRisk(currentPos.cumulativeRisk)
        if (neighbor.cumulativeRisk > updated.cumulativeRisk)
          positions(i._1).update(i._2, updated)
      }
      // updating list of position to visit
      val nextToVisit = (sorted.tail ++ toUpdate).toSet.toList
      discover(positions, nextToVisit)
    }

  def discoverFromOrigin(positions: Array[Array[Position]]): Array[Array[Position]] =
    val origin = Position(0, 0)
    positions(0).update(0, origin)
    discover(positions, List((0, 0)))
    positions

  def destinationRisk(positions: Array[Array[Position]]): Int =
    val x = positions.length - 1
    val y = positions(x).length - 1
    positions(x)(y).cumulativeRisk

  def destinationRisk(lines: List[String]): Int = {
    val positions = discoverFromOrigin(Position.parsePositions(lines))
    destinationRisk(positions)
  }

  def tileRisk(risk: Int, x: Int, y: Int) =
    val r = (risk + x + y) % 9
    if (r == 0)
      9
    else
      r

  def fullPositions(positions: Array[Array[Position]]): Array[Array[Position]] = {
    val repeat = 5
    val xSize = positions.length
    val ySize = positions(0).length

    Array.tabulate(xSize * repeat, ySize * repeat) { (x, y) =>
      val ref = positions(x % xSize)(y % ySize)
      Position(tileRisk(ref.risk, x / xSize, y / ySize))
    }
  }

  def fullDestinationRisk(lines: List[String]): Int = {
    val ps = fullPositions(Position.parsePositions(lines))
    val positions = discoverFromOrigin(ps)
    destinationRisk(positions)
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

  def part2 = println(
    s"This is taking time, but we know that the minimal full destination risk is ${fullDestinationRisk(lines)}"
  )

}
