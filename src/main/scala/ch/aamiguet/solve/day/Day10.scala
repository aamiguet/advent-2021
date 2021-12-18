package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day

object Day10 extends Day {
  val dayId = 10

  lazy val chunks =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList

  val charMap: Map[Char, Char] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>',
  )

  val charValue: Map[Char, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  def illegalCharacter(chunk: String, expectedClosingChars: List[Char] = Nil): Option[Char] =
    if (chunk.isEmpty) {
      None
    } else {
      if (charMap.isDefinedAt(chunk.head)) {
        illegalCharacter(chunk.tail, charMap(chunk.head) :: expectedClosingChars)
      } else {
        if (chunk.head != expectedClosingChars.head)
          Some(chunk.head)
        else
          illegalCharacter(chunk.tail, expectedClosingChars.tail)
      }
    }

  def syntaxErrorScore(chunks: List[String]): Int =
    chunks.map(illegalCharacter(_)).filter(_.isDefined).map(_.get).map(charValue.get(_).get).sum

  def part1 = println(s"Syntax error score is ${syntaxErrorScore(chunks)}")
  def part2 = ???
}
