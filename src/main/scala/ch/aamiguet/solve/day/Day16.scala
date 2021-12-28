package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day

object Day16 extends Day {
  val dayId = 16

  trait Packet {
    val version: Int
    val id: Int
    def subpackets: List[Packet]

    def versionSum: Int = version + subpackets.map(_.versionSum).sum
  }

  case class Frame(val subpackets: List[Packet]) extends Packet {
    val version = 0
    val id = 0
  }

  case class Literal(val version: Int, val id: Int, val value: BigInt) extends Packet {
    val subpackets = Nil
  }

  case class Operator(val version: Int, val id: Int, val subpackets: List[Packet]) extends Packet {}

  case object Packet {

    lazy val zeroes = "^0*$".r

    def parsePacket(binary: String): (Packet, String) = {
      val version = Integer.parseInt(binary.slice(0, 3), 2)
      val id = Integer.parseInt(binary.slice(3, 6), 2)
      val rest = binary.drop(6)

      id match {
        case 4 =>
          parseLiteral(version, id, rest)
        case _ =>
          parseOperator(version, id, rest)
      }
    }

    def parseLiteral(version: Int, id: Int, binary: String): (Literal, String) = {
      def parseValue(build: String, binary: String): (BigInt, String) = {
        val (chunk, rest) = binary.splitAt(5)
        val newBuild = build + chunk.tail
        if (chunk.head == '1')
          parseValue(newBuild, rest)
        else
          (BigInt(newBuild, 2), rest)
      }

      val (v, r) = parseValue("", binary)
      (Literal(version, id, v), r)
    }

    def parseOperator(version: Int, id: Int, binary: String): (Operator, String) = {
      def parseSubpackets0(binary: String): List[Packet] =
        if (binary.isEmpty) Nil
        else {
          val (p, r) = parsePacket(binary)
          p :: parseSubpackets0(r)
        }

      def parseSubpackets1(binary: String, acc: List[Packet], limit: Int): (List[Packet], String) =
        if (limit == 0) (acc.reverse, binary)
        else {
          val (p, r) = parsePacket(binary)
          parseSubpackets1(r, p :: acc, limit - 1)
        }

      binary.head match {
        case '0' =>
          val (length, right) = binary.tail.splitAt(15)
          val (subpacketsBinary, rest) = right.splitAt(Integer.parseInt(length, 2))
          val sps = parseSubpackets0(subpacketsBinary)
          (Operator(version, id, sps), rest)
        case '1' =>
          val (limit, right) = binary.tail.splitAt(11)
          val (sps, rest) = parseSubpackets1(right, Nil, Integer.parseInt(limit, 2))
          (Operator(version, id, sps), rest)
      }
    }

    def parseFrame(binary: String): Frame = {
      def parseSubpackets(binary: String, acc: List[Packet]): List[Packet] =
        if (zeroes.matches(binary)) acc.reverse
        else {
          val (p, r) = parsePacket(binary)
          parseSubpackets(r, p :: acc)
        }

      Frame(parseSubpackets(binary, Nil))
    }

  }

  def leftPad(str: String) = (List.fill(4 - str.length)('0') mkString "") + str

  def toBinary(hex: String) =
    hex.split("").toList.map(h => leftPad(Integer.parseInt(h, 16).toBinaryString)) mkString ""

  def frame(hex: String) = Packet.parseFrame(toBinary(hex))

  lazy val hex =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList
      .head

  lazy val f = frame(hex)

  def part1 = println(s"Sum of all version number is ${f.versionSum}")
  def part2 = ???

}
