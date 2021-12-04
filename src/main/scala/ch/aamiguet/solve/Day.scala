package ch.aamiguet.solve

trait Day {

  def inputFile: String

  def filename: String = s"ressources/$inputFile"

  def part1: Unit
  def part2: Unit

  def solve: Unit = {
    part1
    part2
  }

}
