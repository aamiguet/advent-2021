package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

import Day20.*

class Day20Spec extends Specification {
  "Day 20 Specification".br

  val (algoritm, picture) = parse(
    List(
      "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#",
      "",
      "#..#.",
      "#....",
      "##..#",
      "..#..",
      "..###",
    )
  )

  "Enhancing twice" >> {
    val (p, _) = enhance(picture, algoritm, 2, Black)
    println(print(p))
    litCount(p) mustEqual 35
  }

  "Enhancing 50 times" >> {
    val (p, _) = enhance(picture, algoritm, 50, Black)
    println(print(p))
    litCount(p) mustEqual 3351
  }

}
