package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.io.Source

case class DisplayWire(val codes: List[String], val values: List[String])

object DisplayWire {

  def apply(line: String): DisplayWire = {
    val sp = line.split("\\s\\|\\s")
    DisplayWire(sp(0).split("\\s+").toList, sp(1).split("\\s+").toList)
  }

}

case class Display(
  val top: String,
  val topLeft: String,
  val topRight: String,
  val middle: String,
  val bottomLeft: String,
  val bottomRight: String,
  val bottom: String,
) {
  val zero = (top + topLeft + topRight + bottomLeft + bottomRight + bottom).sorted
  val one = (topRight + bottomRight).sorted
  val two = (top + topRight + middle + bottomLeft + bottom).sorted
  val three = (top + topRight + middle + bottomRight + bottom).sorted
  val four = (topLeft + topRight + middle + bottomRight).sorted
  val five = (top + topLeft + middle + bottomRight + bottom).sorted
  val six = (top + topLeft + middle + bottomLeft + bottomRight + bottom).sorted
  val seven = (top + topRight + bottomRight).sorted
  val eight = (top + topLeft + topRight + middle + bottomLeft + bottomRight + bottom).sorted
  val nine = (top + topLeft + topRight + middle + bottomRight + bottom).sorted

  val code: Map[Int, String] = Map(
    0 -> zero,
    1 -> one,
    2 -> two,
    3 -> three,
    4 -> four,
    5 -> five,
    6 -> six,
    7 -> seven,
    8 -> eight,
    9 -> nine,
  )

  val reverseCode = code.map(e => e._2 -> e._1)

}

object Display {

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

  def apply(codes: List[String]): Display = {
    val dm = digitMap(codes, Map.empty[Int, String])

    val top = dm.get(7).get diff dm.get(1).get
    val d9 = dm.get(4).get + top
    val bottom = codes.map(c => c diff d9).filter(_.length == 1).head
    val nine = d9 + bottom
    val d3 = dm.get(1).get + top + bottom
    val middle = codes.map(c => c diff d3).filter(_.length == 1).head
    val topLeft = dm.get(4).get diff dm.get(1).get diff middle
    val bottomLeft = dm.get(8).get diff nine
    val d5 = top + topLeft + middle + bottom
    val bottomRight = codes.map(c => c diff d5).filter(_.length == 1).head
    val topRight = dm.get(1).get diff bottomRight
    Display(top, topLeft, topRight, middle, bottomLeft, bottomRight, bottom)
  }

}

object Day8 extends Day {
  val dayId = 8

  lazy val wires = Source
    .fromFile(filename)
    .getLines
    .toList
    .map(DisplayWire.apply)

  def easyDigitsCount(wires: List[DisplayWire]): Int = {
    val numbers = List(1, 4, 7, 8)
    wires.map { w =>
      val display = Display.apply(w.codes)
      numbers.foldLeft(0)((acc, n) =>
        w.values.filter(v => v.sorted == display.code.get(n).get).length + acc
      )
    }.sum
  }

  def allDigitSum(wires: List[DisplayWire]): Int =
    wires.map { w =>
      val display = Display.apply(w.codes)
      w.values.map(v => display.reverseCode.get(v.sorted).get.toString).mkString("").toInt
    }.sum

  def part1 = println(s"Count of easy numbers is ${easyDigitsCount(wires)}")
  def part2 = println(s"Sum of all display ${allDigitSum(wires)}")
}
