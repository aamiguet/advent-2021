package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.io.Source

enum Direction(val name: String):
  case Forward extends Direction("Forward")
  case Up extends Direction("Up")
  case Down extends Direction("Down")

case class Move(direction: Direction, units: Int)

object Move {

  def apply(line: String): Move = {
    val split = line.split(" ")
    Move(Direction.valueOf(split(0).capitalize), split(1).toInt)
  }

}

case class Position(horizontal: Int, depth: Int, aim: Int = 0) {

  def next(m: Move): Position =
    m.direction match {
      case Direction.Forward =>
        Position(this.horizontal + m.units, this.depth)
      case Direction.Up =>
        Position(this.horizontal, this.depth - m.units)
      case Direction.Down =>
        Position(this.horizontal, this.depth + m.units)
    }

  def nextWithAim(m: Move): Position =
    m.direction match {
      case Direction.Forward =>
        Position(this.horizontal + m.units, this.depth + this.aim * m.units, this.aim)
      case Direction.Up =>
        Position(this.horizontal, this.depth, this.aim - m.units)
      case Direction.Down =>
        Position(this.horizontal, this.depth, this.aim + m.units)
    }

  def product = this.horizontal * this.depth

}

case object Day2 extends Day {

  val inputFile = "day2.txt"
  val originalPos = Position(0, 0)

  def finalPosition(initialPos: Position, moves: List[Move]): Position =
    moves.foldLeft(initialPos)((p, m) => p.next(m))

  def finalPositionWithAim(initialPos: Position, moves: List[Move]): Position =
    moves.foldLeft(initialPos)((p, m) => p.nextWithAim(m))

  def parseMoves(lines: List[String]): List[Move] = lines.map(Move.apply)

  lazy val readFile: List[Move] = {
    val lines = Source.fromFile(filename).getLines.toList
    parseMoves(lines)
  }

  def part1: Unit = {
    val moves = readFile
    val finalPos = finalPosition(originalPos, moves)
    println(
      s"Part I : the product of the horizontal position and the depth after the moves is ${finalPos.product}"
    )
  }

  def part2: Unit = {
    val moves = readFile
    val finalPosWithAim = finalPositionWithAim(originalPos, moves)
    println(
      s"Part II : the product of the horizontal position and the depth after the moves with aim is ${finalPosWithAim.product}"
    )
  }

}
