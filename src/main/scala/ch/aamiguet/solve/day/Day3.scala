package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.io.Source
import scala.math.pow

object Day3 extends Day {
  val dayId = 3

  lazy val lines = toInt(Source.fromFile(filename).getLines.toList)

  def toInt(line: String): List[Int] = line.split("").map(_.toInt).toList

  def toInt(lines: List[String]): List[List[Int]] = lines.map(toInt)

  def toInt(bits: List[Int]): Int = Integer.parseInt(bits.map(_.toString) mkString "", 2)

  def gasRating(
    lines: List[List[Int]],
    bits: List[Int] = Nil,
  )(
    compare: (ones: Int, zeroes: Int) => Boolean
  ): Int =
    lines match {
      case head :: Nil =>
        toInt(bits ::: head)
      case _ =>
        val map = lines.groupBy(_.head)
        val ones = map.get(1).getOrElse(Nil)
        val zeroes = map.get(0).getOrElse(Nil)

        val (values, bit) =
          if (compare(ones.length, zeroes.length))
            (ones, 1)
          else
            (zeroes, 0)
        val newLines = values.map(_.tail)

        gasRating(newLines, bits ::: List(bit))(compare)
    }

  def co2Rating(lines: List[List[Int]], bits: List[Int] = Nil): Int =
    gasRating(lines, bits)((ones, zeroes) => ones < zeroes)

  def oygenRating(lines: List[List[Int]], bits: List[Int] = Nil): Int =
    gasRating(lines, bits)((ones, zeroes) => ones >= zeroes)

  def rates(lines: List[List[Int]]): (Int, Int) = {
    val init = List.fill(lines.head.length)(0)
    val total = lines.foldLeft(init)((acc, line) => line.zip(acc).map((x, y) => x + y))
    val mid = lines.length / 2
    val bits =
      total.map(n =>
        if (n >= mid)
          "1"
        else
          "0"
      ) mkString ""
    val power = bits.length
    val gamma = Integer.parseInt(bits, 2)
    (gamma, (pow(2d, power) - 1d - gamma.toDouble).toInt)
  }

  def part1 = {
    val (gamma, epsilon) = rates(lines)
    println(s"The product of the gamma ($gamma) and espilon ($epsilon) rates is ${gamma * epsilon}")
  }

  def part2 = {
    val oxygen = oygenRating(lines)
    val co2 = co2Rating(lines)

    println(
      s"The product of the oxygen rating ($oxygen) and the CO2 scrubber rating ($co2) is ${oxygen * co2}"
    )
  }

}
