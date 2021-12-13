package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day8Spec extends Specification {

  "Day 8 specification".br

  val single = DisplayWire(
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |cdfeb fcadb cdfeb cdbaf"
  )

  val dw = List(
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |fgae cfgab fg bagce",
  ).map(DisplayWire.apply)

  "Raw decode" >> {
    Day8.decode("ab", Map.empty[Int, String]) mustEqual Some(1)
  }

  "Single line, simple test" >> {
    val dm = Day8.digitMap(single.codes, Map.empty[Int, String])
    dm.get(1) mustEqual Some("ab")
    dm.get(7) mustEqual Some("dab")
  }

  "Easy count of single" >> {
    Day8.easyDigitsCount(List(single)) mustEqual 0
  }

  "Count" >> {
    val codes = List(
      "edbfga",
      "begcd",
      "cbg",
      "gc",
      "gcadebf",
      "fbgde",
      "acbgfd",
      "abcde",
      "gfcbed",
      "gfec",
    )
    val dm = Map(
      1 -> "gc",
      7 -> "cbg",
      4 -> "gfec",
      8 -> "gcadebf",
    )
    val values = List(
      "fcgedb",
      "cgb",
      "dgebacf",
      "gc",
    )
    Day8.count(values, dm, List(1, 4, 7, 8)) mustEqual 3
  }

  "Count of easy numbers" >> {
    Day8.easyDigitsCount(dw) mustEqual 26
  }

}
