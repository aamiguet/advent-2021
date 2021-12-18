package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day10Spec extends Specification {

  "Day 10 Specification".br

  val chunks = List(
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]",
  )

  "Illegal character testing with syntax error" >> {
    Day10.illegalCharacter("{([(<{}[<>[]}>{[]{[(<()>") mustEqual Some('}')
  }

  "Illegal character testing with ok chunck" >> {
    Day10.illegalCharacter("[({(<(())[]>[[{[]{<()<>>") mustEqual None
  }

  "Syntax error score" >> {
    Day10.syntaxErrorScore(chunks) mustEqual 26397
  }
}
