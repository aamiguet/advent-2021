package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day

object Day13 extends Day {

  case class Position(val x: Int, val y: Int)

  object Position {

    def apply(line: String): Position = {
      val s = line.split(",").map(_.toInt)
      Position(s(0), s(1))
    }

  }

  sealed trait Fold

  case class AlongX(val x: Int) extends Fold
  case class AlongY(val y: Int) extends Fold

  object Fold {

    def apply(description: String): Fold = {
      val s = description.split("=");
      if (s(0).endsWith("y"))
        AlongY(s(1).toInt)
      else
        AlongX(s(1).toInt)
    }

  }

  val dayId = 13

  lazy val lines =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList

  lazy val dots = lines.takeWhile(!_.isEmpty).map(Position.apply).toSet
  lazy val folds = lines.drop(dots.size + 1).map(Fold.apply)

  def filterDots(dot: Position, fold: Fold): Boolean =
    fold match {
      case AlongX(x) =>
        dot.x != x
      case AlongY(y) =>
        dot.y != y
    }

  def mapDots(dot: Position, fold: Fold): Position =
    fold match {
      case AlongX(x) if dot.x > x =>
        Position(x - (dot.x - x), dot.y)
      case AlongY(y) if dot.y > y =>
        Position(dot.x, y - (dot.y - y))
      case _ =>
        dot
    }

  def foldPaper(
    dots: Set[Position],
    fold: Fold,
  ): Set[Position] = dots.filter(filterDots(_, fold)).map(mapDots(_, fold))

  def foldPaper(
    dots: Set[Position],
    folds: List[Fold],
  ): Set[Position] = folds.foldLeft(dots)((acc, f) => foldPaper(acc, f))

  def printDots(
    dots: Set[Position]
  ): Unit = {
    val width = dots.foldLeft(0)((acc, d) => Math.max(acc, d.x))
    val height = dots.foldLeft(0)((acc, d) => Math.max(acc, d.y))
    val rangeX = (0 to width).toList
    val rangeY = (0 to height).toList

    val picture =
      rangeY.map(y =>
        rangeX.map { x =>
          if (dots.contains(Position(x, y)))
            "#"
          else
            " "
        } mkString ""
      ) mkString "\n"
    println(picture)
  }

  def part1 = {
    val d1 = foldPaper(dots, folds.head)
    println(s"Number of dots after a single fold is ${d1.size}")
  }

  def part2 = {
    val finalDots = foldPaper(dots, folds)
    printDots(finalDots)
  }

}
