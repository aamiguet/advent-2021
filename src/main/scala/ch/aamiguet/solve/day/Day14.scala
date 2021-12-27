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

  def elementFrequency(polymer: String): Map[Char, Long] =
    polymer.foldLeft(Map.empty[Char, Long]) { (acc, c) =>
      acc.updatedWith(c) {
        case Some(n) =>
          Some(n + 1L)
        case None =>
          Some(1L)
      }
    }

  def pairMap(polymer: String): Map[String, Long] = {
    val zipped = polymer.zip(polymer.tail)
    zipped.map((l, r) => s"$l$r").groupBy(identity).mapValues(_.size.toLong).toMap
  }

  def polymerization(pair: String, rules: Map[String, Char]): List[String] =
    rules.get(pair) match {
      case Some(c) =>
        List(s"${pair.head}$c", s"$c${pair.last}")
      case None =>
        List.empty[String]
    }

  def polymerization(
    pairMap: Map[String, Long],
    rules: Map[String, Char],
    steps: Int,
  ): Map[String, Long] =
    if (steps == 0) pairMap
    else {
      val pm =
        pairMap.foldLeft(Map.empty[String, Long]) { (newPm, e) =>
          polymerization(e._1, rules).foldLeft(newPm) { (acc, pair) =>
            acc.updatedWith(pair) {
              case Some(c) =>
                Some(c + e._2)
              case None =>
                Some(e._2)
            }
          }
        }

      polymerization(pm, rules, steps - 1)
    }

  def commonCount(ef: Map[Char, Long]): Long = {
    val sorted = ef.toList.sortWith((f1, f2) => f1._2 > f2._2)
    sorted.head._2 - sorted.last._2
  }

  def pairCount(pairMap: Map[String, Long], terminator: Char): Long = {
    val ef =
      pairMap
        .foldLeft(Map.empty[Char, Long]) { (acc, p) =>
          acc.updatedWith(p._1.head) {
            case Some(c) =>
              Some(c + p._2)
            case None =>
              Some(p._2)
          }
        }
        .updatedWith(terminator) {
          case Some(c) =>
            Some(c + 1L)
          case None =>
            Some(1L)
        }
    commonCount(ef)
  }

  def pairCount(template: String, rules: Map[String, Char], steps: Int): Long = {
    val pm = polymerization(pairMap(template), rules, steps)
    pairCount(pm, template.last)
  }

  def commonCount(template: String, rules: Map[String, Char], steps: Int): Long = {
    val polymer = polymerization(template, rules, steps)
    val ef = elementFrequency(polymer)
    commonCount(ef)
  }

  def part1Count(
    template: String,
    rules: Map[String, Char],
  ): Long = commonCount(template, rules, 10)

  def part2Count(template: String, rules: Map[String, Char]): Long = pairCount(template, rules, 40)

  def part1 = {
    val c = part1Count(template, rules)
    println(
      s"Substracting the quantity of least common element from the quantity of the most common element gives us ${c}"
    )
  }

  def part2 = {
    val c = part2Count(template, rules)
    println(
      s"A 40 generation polymer is very very large, the difference is ${c}"
    )
  }

}
