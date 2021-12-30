package ch.aamiguet.solve.day

import ch.aamiguet.solve.Day

object Day18 extends Day {
  val dayId = 18

  sealed trait SnailfishNumber {
    def +(s: SnailfishNumber): SnailfishNumber
    def explode(depth: Int): ExplodeMessage
    def split: SplitMessage
    def regularValue: Int
    def addLeft(v: Int): SnailfishNumber
    def addRight(v: Int): SnailfishNumber
    def magnitude: Int
  }

  sealed trait ExplodeMessage

  final case class Explosion(
    val sn: SnailfishNumber,
    leftValue: Option[Int],
    rightValue: Option[Int],
  ) extends ExplodeMessage

  object NoExplosion extends ExplodeMessage

  sealed trait SplitMessage

  final case class Split(
    val sn: SnailfishNumber
  ) extends SplitMessage

  object NoSplit extends SplitMessage

  final case class Pair(val left: SnailfishNumber, val right: SnailfishNumber)
    extends SnailfishNumber {
    def +(s: SnailfishNumber) = Pair(this, s).reduce

    def explode(depth: Int): ExplodeMessage =
      if (depth == 5) Explosion(Regular(0), Some(left.regularValue), Some(right.regularValue))
      else {
        val deeper = depth + 1
        left.explode(deeper) match {
          case Explosion(newLeft, leftVal, rightVal) =>
            val newRight = rightVal.map(v => right.addLeft(v)).getOrElse(right)
            Explosion(Pair(newLeft, newRight), leftVal, None)
          case NoExplosion =>
            right.explode(deeper) match {
              case Explosion(newRight, leftVal, rightVal) =>
                val newLeft = leftVal.map(v => left.addRight(v)).getOrElse(left)
                Explosion(Pair(newLeft, newRight), None, rightVal)
              case NoExplosion =>
                NoExplosion
            }
        }
      }

    def split: SplitMessage =
      left.split match {
        case Split(l) =>
          Split(Pair(l, right))
        case NoSplit =>
          right.split match {
            case Split(r) =>
              Split(Pair(left, r))
            case NoSplit =>
              NoSplit
          }
      }

    def reduce: SnailfishNumber =
      explode(1) match {
        case Explosion(sn: Pair, _, _) =>
          sn.reduce
        case Explosion(_, _, _) =>
          throw new Exception("Undefined")
        case NoExplosion =>
          splitReduce
      }

    def splitReduce: SnailfishNumber =
      split match {
        case Split(sn: Pair) =>
          sn.reduce
        case Split(_) =>
          throw new Exception("Undefined")
        case NoSplit =>
          this
      }

    def addLeft(v: Int): SnailfishNumber = Pair(left.addLeft(v), right)

    def addRight(v: Int): SnailfishNumber = Pair(left, right.addRight(v))
    def regularValue = throw new Exception("Undefined")

    def magnitude = 3 * left.magnitude + 2 * right.magnitude

    override def toString = s"[$left, $right]"
  }

  final case class Regular(val value: Int) extends SnailfishNumber {

    def +(s: SnailfishNumber) =
      s match {
        case Regular(v) =>
          Regular(value + v)
        case Pair(_, _) =>
          throw new Exception("Undefined operation")
      }

    def explode(depth: Int) = NoExplosion

    def split: SplitMessage =
      if (value >= 10)
        Split(
          Pair(Regular(math.floor(value / 2d).toInt), Regular(math.ceil(value / 2d).toInt))
        )
      else
        NoSplit

    def regularValue = value

    def addLeft(v: Int): SnailfishNumber = Regular(value + v)

    def addRight(v: Int): SnailfishNumber = Regular(value + v)

    def magnitude = value

    override def toString = value.toString
  }

  object SnailfishNumber {

    def apply(str: String): SnailfishNumber =
      def leftAndRight(left: String, right: String, openCount: Int): (String, String) =
        right.head match {
          case '[' =>
            leftAndRight(left + right.head.toString, right.tail, openCount + 1)
          case ']' =>
            leftAndRight(left + right.head.toString, right.tail, openCount - 1)
          case ',' if openCount == 0 =>
            (left, right.tail)
          case _ =>
            leftAndRight(left + right.head.toString, right.tail, openCount)
        }

      if (str.head == '[') {
        val (left, right) = leftAndRight("", str.tail.dropRight(1), 0)
        Pair(SnailfishNumber(left), SnailfishNumber(right))
      } else {
        Regular(str.toInt)
      }

    def sum(sns: List[SnailfishNumber]): SnailfishNumber =
      def loop(current: SnailfishNumber, sns: List[SnailfishNumber]): SnailfishNumber =
        if (sns.isEmpty)
          current
        else {
          loop((current + sns.head), sns.tail)
        }
      sns.tail.fold(sns.head)(_ + _)

    def parse(lines: List[String]): List[SnailfishNumber] = lines.map(SnailfishNumber.apply(_))
  }

  lazy val lines =
    scala
      .io
      .Source
      .fromFile(filename)
      .getLines
      .toList

  lazy val sns = SnailfishNumber.parse(lines)

  def maxSumOfAny2(sns: List[SnailfishNumber]): Int = {
    val pairs = for {
      s1 <- sns
      s2 <- sns if s1 != s2
    } yield (s1, s2)

    pairs.map(p => (p._1 + p._2).magnitude).max
  }

  def part1 = println(s"Magnitude of the final sum is ${SnailfishNumber.sum(sns).magnitude}")
  def part2 = println(s"Largest magnitude is ${maxSumOfAny2(sns)}")
}
