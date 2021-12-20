package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

class Day12Spec extends Specification {

  "Day 12 Specification".br

  val startCave = StartCave
  val endCave = EndCave
  val A = LargeCave("A")
  val b = SmallCave("b")
  val c = SmallCave("c")
  val d = SmallCave("d")

  val connections = Map(
    startCave -> List(A, b),
    A -> List(startCave, c, b, endCave),
    b -> List(startCave, A, d, endCave),
    c -> List(A),
    d -> List(b),
    endCave -> List(A, b),
  )

  val largerSystemLines = List(
    "dc-end",
    "HN-start",
    "start-kj",
    "dc-start",
    "dc-HN",
    "LN-dc",
    "HN-end",
    "kj-sa",
    "kj-HN",
    "kj-dc",
  )

  val evenLargerSystemLines = List(
    "fs-end",
    "he-DX",
    "fs-he",
    "start-DX",
    "pj-DX",
    "end-zg",
    "zg-sl",
    "zg-pj",
    "pj-he",
    "RW-he",
    "fs-DX",
    "pj-RW",
    "zg-RW",
    "start-pj",
    "he-WI",
    "zg-he",
    "pj-fs",
    "start-RW",
  )

  "Small system" >> {
    CaveSystem(connections).allPaths.size mustEqual 10
  }

  "Cave parsing" >> {
    Cave("end") mustEqual EndCave
    Cave("start") mustEqual StartCave
    Cave("ca") mustEqual SmallCave("ca")
    Cave("VS") mustEqual LargeCave("VS")
  }

  "Larger system" >> {
    CaveSystem(largerSystemLines).allPaths.size mustEqual 19
  }

  "Even larger system" >> {
    CaveSystem(evenLargerSystemLines).allPaths.size mustEqual 226
  }

}
