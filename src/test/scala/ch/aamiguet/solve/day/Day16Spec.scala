package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

import Day16.*

class Day16Spec extends Specification {
  "Day 16 Specification".br

  "Binary conversion" >> {
    toBinary("D2FE28") mustEqual "110100101111111000101000"
  }

  "Parse literal" >> {
    Packet.parsePacket("110100101111111000101000")._1 mustEqual Literal(6, 4, 2021)
  }

  "Parse operator of type 0" >> {
    Packet.parsePacket("00111000000000000110111101000101001010010001001000000000")._1 mustEqual
      Operator(1, 6, List(Literal(6, 4, 10), Literal(2, 4, 20)))
  }

  "Parse operator of type 1" >> {
    Packet.parsePacket("11101110000000001101010000001100100000100011000001100000")._1 mustEqual
      Operator(7, 3, List(Literal(2, 4, 1), Literal(4, 4, 2), Literal(1, 4, 3)))
  }

  "Testing some frames" >> {
    frame("8A004A801A8002F478").versionSum mustEqual 16
    frame("620080001611562C8802118E34").versionSum mustEqual 12
    frame("C0015000016115A2E0802F182340").versionSum mustEqual 23
    frame("A0016C880162017C3686B18A3D4780").versionSum mustEqual 31
  }

  "Computing some values" >> {
    frame("C200B40A82").computedValue mustEqual 3
    frame("04005AC33890").computedValue mustEqual 54
    frame("880086C3E88112").computedValue mustEqual 7
    frame("CE00C43D881120").computedValue mustEqual 9
    frame("D8005AC2A8F0").computedValue mustEqual 1
    frame("F600BC2D8F").computedValue mustEqual 0
    frame("9C005AC2F8F0").computedValue mustEqual 0
    frame("9C0141080250320F1802104A08").computedValue mustEqual 1
  }

}
