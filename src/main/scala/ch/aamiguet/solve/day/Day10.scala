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

  val charValueError: Map[Char, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  val charValueMissing: Map[Char, Long] = Map(
    ')' -> 1L,
    ']' -> 2L,
    '}' -> 3L,
    '>' -> 4L,
  )

  def missingClosingChars(
    chunk: String,
    expectedClosingChars: List[Char] = Nil,
  ): Either[Char, List[Char]] =
    if (chunk.isEmpty) {
      Right(expectedClosingChars)
    } else {
      if (charMap.isDefinedAt(chunk.head)) {
        missingClosingChars(chunk.tail, charMap(chunk.head) :: expectedClosingChars)
      } else {
        if (chunk.head != expectedClosingChars.head)
          Left(chunk.head)
        else
          missingClosingChars(chunk.tail, expectedClosingChars.tail)
      }
    }

  def syntaxErrorScore(chunks: List[String]): Int =
    chunks
      .map(missingClosingChars(_))
      .filter(_.isLeft)
      .map(_.swap.toOption.get)
      .map(charValueError.get(_).get)
      .sum

  def missingScore(closingChars: List[Char]): Long =
    closingChars.foldLeft(0L)((acc, c) => acc * 5 + charValueMissing.get(c).get)

  def missingClosingCharsScore(chunks: List[String]): Long = {
    val scores =
      chunks
        .map(missingClosingChars(_))
        .filter(_.isRight)
        .map(_.toOption.get)
        .map(missingScore(_))
        .sorted
    scores.apply(scores.length / 2)
  }

  def part1 = println(s"Syntax error score is ${syntaxErrorScore(chunks)}")
  def part2 = println(s"Missing closing chars score is ${missingClosingCharsScore(chunks)}")
}
