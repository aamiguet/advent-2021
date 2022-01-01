package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day

object Day20 extends Day {
  val dayId = 20

  val BlackStr = '.'
  val WhiteStr = '#'

  trait Pixel {
    val toBit: Char
    val toStr: Char

    override def toString = toStr.toString
  }

  case object Black extends Pixel {
    val toBit = '0'
    val toStr = '.'
  }

  case object White extends Pixel {
    val toBit = '1'
    val toStr = '#'
  }

  object Pixel {

    def apply(c: Char): Pixel =
      c match {
        case WhiteStr =>
          White
        case BlackStr =>
          Black
      }

    def toInt(pixels: List[Pixel]): Int = Integer.parseInt(pixels.map(_.toBit).mkString, 2)
  }

  type Picture = Array[Array[Pixel]]

  def parsePicture(lines: List[String]): Picture =
    lines.map(line => line.toArray.map(Pixel.apply)).toArray

  def parse(lines: List[String]): (String, Picture) = (lines.head, parsePicture(lines.drop(2)))

  def enhance(pixels: List[Pixel], algorithm: String): Pixel = {
    val index = Pixel.toInt(pixels)
    Pixel(algorithm(index))
  }

  def print(picture: Picture): String = {
    val x = picture.toList.map(_.toList)
    x.map(line => (line mkString "")) mkString "\n"
  }

  def litCount(picture: Picture): Int = picture.map(_.filter(_ == White).size).sum

  def enhance(picture: Picture, algorithm: String, default: Pixel): (Picture, Pixel) = {
    val newArr =
      Array.tabulate(picture.length + 2, picture(0).length + 2) { (n, m) =>
        (for {
          i <- -1 to 1
          j <- -1 to 1
        } yield picture.lift(n - 1 + i).flatMap(p => p.lift(m - 1 + j)).getOrElse(default)).toList
      }
    val newDefault = enhance(List.fill(9)(default), algorithm)
    (newArr.map(_.map(ps => enhance(ps, algorithm))), newDefault)
  }

  def enhance(picture: Picture, algorithm: String, times: Int, default: Pixel): (Picture, Pixel) =
    if (times == 0) (picture, default)
    else {
      val (p, nd) = enhance(picture, algorithm, default)
      enhance(p, algorithm, times - 1, nd)
    }

  lazy val lines =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList

  lazy val (algoritm, picture) = parse(lines)

  def part1 = {
    val (ep, _) = enhance(picture, algoritm, 2, Black)
    val lc = litCount(ep)
    println(s"Enhancing twice gives ${lc} lit pixels")
  }

  def part2 = {
    val (ep, _) = enhance(picture, algoritm, 50, Black)
    val lc = litCount(ep)
    println(s"Enhancing twice gives ${lc} lit pixels")
  }
}
