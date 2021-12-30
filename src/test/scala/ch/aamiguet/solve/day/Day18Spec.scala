package ch.aamiguet.solve.day

import org.specs2.mutable.Specification

import Day18.*

class Day18Spec extends Specification {
  "Day 18 Specification".br

  val shortList = SnailfishNumber.parse(
    List(
      "[1,1]",
      "[2,2]",
      "[3,3]",
      "[4,4]",
    )
  )

  val mediumList = SnailfishNumber.parse(
    List(
      "[1,1]",
      "[2,2]",
      "[3,3]",
      "[4,4]",
      "[5,5]",
    )
  )

  val largeList = SnailfishNumber.parse(
    List(
      "[1,1]",
      "[2,2]",
      "[3,3]",
      "[4,4]",
      "[5,5]",
      "[6,6]",
    )
  )

  val largerList = SnailfishNumber.parse(
    List(
      "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
      "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
      "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
      "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
      "[7,[5,[[3,8],[1,4]]]]",
      "[[2,[2,2]],[8,[8,1]]]",
      "[2,9]",
      "[1,[[[9,3],9],[[9,0],[0,7]]]]",
      "[[[5,[7,4]],7],1]",
      "[[[[4,2],2],6],[8,7]]",
    )
  )

  val fullExample = SnailfishNumber.parse(
    List(
      "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
      "[[[5,[2,8]],4],[5,[[9,9],0]]]",
      "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
      "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
      "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
      "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
      "[[[[5,4],[7,7]],8],[[8,3],8]]",
      "[[9,3],[[9,9],[6,[4,9]]]]",
      "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
      "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]",
    )
  )

  "Step by step" >> {
    (largerList.head + largerList.tail.head) mustEqual SnailfishNumber(
      "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
    )
  }

  "Parsing numbers" >> {
    SnailfishNumber("[[[[[9,8],1],2],3],4]") mustEqual Pair(
      Pair(Pair(Pair(Pair(Regular(9), Regular(8)), Regular(1)), Regular(2)), Regular(3)),
      Regular(4),
    )
  }

  "Split" >> {
    Regular(12).split mustEqual Split(Pair(Regular(6), Regular(6)))
    Regular(13).split mustEqual Split(Pair(Regular(6), Regular(7)))
    Regular(9).split mustEqual NoSplit
  }

  "Reduction 1" >> {
    val pair = SnailfishNumber("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]").asInstanceOf[Pair]
    pair.reduce mustEqual SnailfishNumber("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  }

  "Reduction 2" >> {
    val pair = SnailfishNumber("[7,[6,[5,[4,[3,2]]]]]").asInstanceOf[Pair]
    pair.reduce mustEqual SnailfishNumber("[7,[6,[5,[7,0]]]]")
  }

  "Reduction 5" >> {
    val pair = SnailfishNumber("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]").asInstanceOf[Pair]
    pair.reduce mustEqual SnailfishNumber("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  }

  "Sums" >> {
    SnailfishNumber.sum(shortList) mustEqual SnailfishNumber("[[[[1,1],[2,2]],[3,3]],[4,4]]")
    SnailfishNumber.sum(mediumList) mustEqual SnailfishNumber("[[[[3,0],[5,3]],[4,4]],[5,5]]")
    SnailfishNumber.sum(largeList) mustEqual SnailfishNumber("[[[[5,0],[7,4]],[5,5]],[6,6]]")
    SnailfishNumber.sum(largerList) mustEqual SnailfishNumber(
      "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    )
  }

  "Full example" >> {
    val s = SnailfishNumber.sum(fullExample)
    s mustEqual SnailfishNumber("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")
    s.magnitude mustEqual 4140
  }

  "Magnitude" >> {
    SnailfishNumber("[[1,2],[[3,4],5]]").magnitude mustEqual 143
    SnailfishNumber("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").magnitude mustEqual 1384
    SnailfishNumber("[[[[1,1],[2,2]],[3,3]],[4,4]]").magnitude mustEqual 445
    SnailfishNumber("[[[[3,0],[5,3]],[4,4]],[5,5]]").magnitude mustEqual 791
    SnailfishNumber("[[[[5,0],[7,4]],[5,5]],[6,6]]").magnitude mustEqual 1137
    SnailfishNumber(
      "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    ).magnitude mustEqual 3488
  }

  "Largest magnitude of the sum of any two snailfish numbers" >> {
    maxSumOfAny2(fullExample) mustEqual 3993
  }

}
