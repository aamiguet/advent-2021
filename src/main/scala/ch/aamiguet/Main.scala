package ch.aamiguet

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.{ExitCode}

import solve.Solver

object Main extends IOApp {

  def parseArgs(args: List[String], acc: Map[String, Any]): Map[String, Any] = {
    args match {
      case "-d" :: day :: tail => parseArgs(tail, acc + ("day" -> day))
      case "-f" :: filename :: tail => parseArgs(tail, acc + ("file" -> filename))
      case "-o" :: output :: tail => parseArgs(tail, acc + ("output" -> output))
      case _ => acc
    }
  }

  // This is your new "main"!
  def run(args: List[String]): IO[ExitCode] =
    for {
      m <- IO(parseArgs(args, Map.empty))
      _ <- IO.println(m)
      _ <- IO(Solver.solve(m))
    } yield ExitCode.Success
}
