package ch.aamiguet.solve

import day.*

object Solver {

  val dm: Map[String, Day] = Map(
    "1" -> Day1,
    "2" -> Day2,
    "3" -> Day3,
    "4" -> Day4,
    "5" -> Day5,
    "6" -> Day6,
    "7" -> Day7,
    "8" -> Day8,
    "9" -> Day9,
  )

  def solve(args: Map[String, Any]): Unit =
    args.get("day") match {
      case Some(d: String) if dm.keys.toList.contains(d) =>
        dm.get(d).head.solve
      case Some(d: String) if d == "a" || d == "all" =>
        dm.map { (d, solver) =>
          println(s"Solving day $d")
          solver.solve
        }
      case _ =>
    }

}
