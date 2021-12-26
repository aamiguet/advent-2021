package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

import Day14.*

class Day14Spec extends Specification {
  "Day 14 Specification".br

  val template = "NNCB"

  val rules = parseRules(
    List(
      "CH -> B",
      "HH -> N",
      "CB -> H",
      "NH -> C",
      "HB -> C",
      "HC -> B",
      "HN -> C",
      "NN -> C",
      "BH -> H",
      "NC -> B",
      "NB -> B",
      "BN -> B",
      "BB -> N",
      "BC -> B",
      "CC -> N",
      "CN -> C",
    )
  )

  "Single polymerization step" >> {
    polymerizationStep(template, rules) mustEqual "NCNBCHB"
  }

  "Ten steps polymerization" >> {
    polymerization(template, rules, 10).length mustEqual 3073
  }

  "Substracting the quantity of least common element from the quantity of the most common element" >> {
    part1Count(template, rules) mustEqual 1588L
  }

  "Substracting the quantity of least common element from the quantity of the most common element" >> {
    part2Count(template, rules) mustEqual 2188189693529L
  }
}
