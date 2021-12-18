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
    Day10.missingClosingChars("{([(<{}[<>[]}>{[]{[(<()>") mustEqual Left('}')
  }

  "Illegal character testing with ok chunck" >> {
    Day10.missingClosingChars("[({(<(())[]>[[{[]{<()<>>") mustEqual Right(
      List('}', '}', ']', ']', ')', '}', ')', ']')
    )
  }

  "Syntax error score" >> {
    Day10.syntaxErrorScore(chunks) mustEqual 26397
  }

  "Missing chars score" >> {
    Day10.missingClosingCharsScore(chunks) mustEqual 288957L
  }
}
