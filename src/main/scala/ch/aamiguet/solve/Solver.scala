package ch.aamiguet.solve

object Solver {

  def solve(args: Map[String, Any]): Unit = {
    args.get("day") match {
      case Some(d: String)  if d == "1" => day.Day1.solve
      case Some(d: String)  if d == "2" => day.Day2.solve
      case _ =>
    }
  }
}