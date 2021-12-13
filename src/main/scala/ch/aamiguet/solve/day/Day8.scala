package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.io.Source

case class DisplayWire(val codes: List[String], val values: List[String])

object DisplayWire {

  def apply(line: String): DisplayWire = {
    val sp = line.split("\\s\\|")
    DisplayWire(sp(0).split("\\s+").toList, sp(1).split("\\s+").toList)
  }

}

object Day8 extends Day {
  val dayId = 8

  lazy val wires = Source
    .fromFile(filename)
    .getLines
    .toList
    .map(DisplayWire.apply)

  def decode(code: String, dm: Map[Int, String]): Option[Int] =
    code.length match {
      case 2 =>
        Some(1)
      case 3 =>
        Some(7)
      case 4 =>
        Some(4)
      case 7 =>
        Some(8)
      case _ =>
        None
    }

  def digitMap(codes: List[String], dm: Map[Int, String]): Map[Int, String] =
    codes.foldLeft(dm) { (acc, c) =>
      decode(c, dm) match {
        case Some(digit) =>
          acc + (digit -> c)
        case _ =>
          acc
      }
    }

  def count(values: List[String], dm: Map[Int, String], number: Int): Int =
    dm.get(number) match {
      case Some(c) =>
        values.filter(_.sorted == c.sorted).length
      case _ =>
        0
    }

  def count(values: List[String], dm: Map[Int, String], numbers: List[Int]): Int =
    numbers.map(count(values, dm, _)).sum

  def easyDigitsCount(wires: List[DisplayWire]): Int = {
    val numbers = List(1, 4, 7, 8)
    wires.map { w =>
      val dm = digitMap(w.codes, Map.empty[Int, String])
      count(w.values, dm, numbers)
    }.sum
  }

  def part1 = println(s"Count of easy numbers is ${easyDigitsCount(wires)}")
  def part2 = ???
}
