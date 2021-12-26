package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import scala.annotation.tailrec

object Day14 extends Day {
  val dayId = 14

  lazy val lines =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList

  lazy val template = lines.head
  lazy val rules = parseRules(lines.drop(2))

  def parseRules(lines: List[String]): Map[String, Char] =
    lines.map { l =>
      val split = l.split(" -> ")
      (split(0), split(1).head)
    }.toMap

  @tailrec
  def polymerizationStep(template: String, rules: Map[String, Char], acc: String = ""): String =
    if (template.tail.isEmpty) acc + template.head
    else {
      val pair = s"${template.head}${template.tail.head}"
      rules.get(pair) match {
        case Some(c) =>
          polymerizationStep(template.tail, rules, acc + template.head + c)
        case None =>
          polymerizationStep(template.tail, rules, acc + template.head)
      }
    }

  @tailrec
  def polymerization(polymer: String, rules: Map[String, Char], steps: Int): String =
    if (steps == 0) polymer
    else {
      val newPolymer = polymerizationStep(polymer, rules, "")
      polymerization(newPolymer, rules, steps - 1)
    }

  def elementFrequency(polymer: String): Map[Char, Int] =
    polymer.groupBy(identity).mapValues(_.size).toMap

  def part1Count(ef: Map[Char, Int]): Int = {
    val sorted = ef.toList.sortWith((f1, f2) => f1._2 > f2._2)
    sorted.head._2 - sorted.last._2
  }

  def part1Count(template: String, rules: Map[String, Char]): Int = {
    val polymer = polymerization(template, rules, 10)
    val ef = elementFrequency(polymer)
    println(ef)
    part1Count(ef)
  }

  def part1 = {
    val c = part1Count(template, rules)
    println(
      s"Substracting the quantity of least common element from the quantity of the most common element gives us ${c}"
    )
  }

  def part2 = ???
}
