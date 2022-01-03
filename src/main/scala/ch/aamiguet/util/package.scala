package ch.aamiguet

import scala.io.Source.fromFile

package object util {

  def readLines(filename: String) = fromFile(filename).getLines.toList

}
