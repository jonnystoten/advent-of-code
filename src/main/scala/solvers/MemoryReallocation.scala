package solvers

import scala.collection.mutable

class MemoryReallocation extends Solver[Int] {
  def part1(input: String): Int = {
    val banks = input.split('\t').map(_.toInt)

    def findHighestIndex(banks: Array[Int]) = {
      def loop(highest: Int, index: Int): Int =
        if (index == banks.length) highest
        else if (banks(index) > banks(highest)) loop(index, index + 1)
        else loop(highest, index + 1)
      loop(0, 0)
    }

    def redistribute(blocks: Int, banks: Array[Int], index: Int): Unit = {
      if (blocks <= 0) ()
      else {
        banks(index) += 1
        redistribute(blocks - 1, banks, (index + 1) % banks.length)
      }
    }

    def loop(acc: Int, seen: Set[mutable.WrappedArray[Int]]): Int = {
      val highestIndex = findHighestIndex(banks)
      val blocks = banks(highestIndex)
      banks(highestIndex) = 0
      redistribute(blocks, banks, (highestIndex + 1) % banks.length)
      if (seen.contains(wrapIntArray(banks))) acc
      else loop(acc + 1, seen + wrapIntArray(banks.clone))
    }

    loop(1, Set(wrapIntArray(banks.clone)))
  }

  def part2(input: String): Int = {
    val banks = input.split('\t').map(_.toInt)

    def findHighestIndex(banks: Array[Int]) = {
      def loop(highest: Int, index: Int): Int =
        if (index == banks.length) highest
        else if (banks(index) > banks(highest)) loop(index, index + 1)
        else loop(highest, index + 1)
      loop(0, 0)
    }

    def redistribute(blocks: Int, banks: Array[Int], index: Int): Unit = {
      if (blocks <= 0) ()
      else {
        banks(index) += 1
        redistribute(blocks - 1, banks, (index + 1) % banks.length)
      }
    }

    def loop(acc: Int, seen: Set[mutable.WrappedArray[Int]]): Int = {
      val highestIndex = findHighestIndex(banks)
      val blocks = banks(highestIndex)
      banks(highestIndex) = 0
      redistribute(blocks, banks, (highestIndex + 1) % banks.length)
      if (seen.contains(wrapIntArray(banks))) acc
      else loop(acc + 1, seen + wrapIntArray(banks.clone))
    }

    val first = loop(1, Set(wrapIntArray(banks.clone)))
    val second = loop(first + 1, Set(wrapIntArray(banks.clone)))
    second - first
  }
}
