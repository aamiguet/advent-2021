package ch.aamiguet.solve

trait Day {

  val dayId: Int

  def filename: String = s"ressources/day$dayId.txt"

  def part1: Unit
  def part2: Unit

  def solve: Unit = {
    part1
    part2
  }

}
