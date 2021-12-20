package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day
import java.awt.image.SampleModel

trait Cave {
  def isStart: Boolean
  def isEnd: Boolean
  def isSmall: Boolean
  def isLarge: Boolean = !isSmall
}

case class SmallCave(val name: String) extends Cave {
  def isStart = false
  def isEnd = false
  def isSmall = true
}

case class LargeCave(val name: String) extends Cave {
  def isStart = false
  def isEnd = false
  def isSmall = false
}

case object StartCave extends Cave {
  def isStart = true
  def isEnd = false
  def isSmall = false
}

case object EndCave extends Cave {
  def isStart = false
  def isEnd = true
  def isSmall = false
}

object Cave {

  def apply(name: String): Cave =
    name match {
      case "start" =>
        StartCave
      case "end" =>
        EndCave
      case lower if lower.toLowerCase == lower =>
        SmallCave(lower)
      case upper =>
        LargeCave(upper)
    }

}

case class CaveSystem(connections: Map[Cave, List[Cave]], extraSmallCaveVisit: Int) {
  type Path = List[Cave]

  def filterVisit(cave: Cave, currentPath: Path): Boolean =
    cave match {
      case StartCave =>
        false
      case LargeCave(_) =>
        true
      case SmallCave(_) if !currentPath.contains(cave) =>
        true
      case SmallCave(_) =>
        currentPath
          .filter(_.isSmall)
          .groupBy(identity)
          .mapValues(vs => vs.size - 1)
          .values
          .sum < extraSmallCaveVisit
      case _ =>
        true
    }

  def paths(currentPath: Path): List[Path] = {
    val currentCave = currentPath.head
    if (currentCave == EndCave)
      List(currentPath)
    else {
      connections.getOrElse(currentCave, Nil) match {
        case Nil =>
          Nil
        case caves =>
          caves
            .filter(c => filterVisit(c, currentPath))
            .flatMap(c => paths(c :: currentPath))
      }
    }
  }

  def allPaths: Set[Path] = {
    val startingPaths = connections.getOrElse(StartCave, Nil).map(c => List(c, StartCave))
    startingPaths.flatMap(paths(_)).toSet
  }

}

object CaveSystem {

  def apply(connections: List[String], extraSmallCaveVisit: Int = 0): CaveSystem = {
    val cs =
      connections.foldLeft(Map.empty[Cave, List[Cave]]) { (acc, c) =>
        val split = c.split("-")
        val c1 = Cave(split(0))
        val c2 = Cave(split(1))
        acc.updated(c1, c2 :: acc.getOrElse(c1, Nil)).updated(c2, c1 :: acc.getOrElse(c2, Nil))
      }
    CaveSystem(cs, extraSmallCaveVisit)
  }

}

object Day12 extends Day {
  val dayId = 12

  lazy val lines =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList

  def part1 = println(s"The number of paths is ${CaveSystem(lines).allPaths.size}")

  def part2 = println(
    s"The number of paths with a single extra visit is ${CaveSystem(lines, 1).allPaths.size}"
  )

}
