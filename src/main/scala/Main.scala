import solvers._

import scala.io.Source

object Main extends App {
  val day = args(0).toInt
  var input = Source.fromFile(s"input/$day.txt").mkString.trim
  val solver = day match {
    case 1 => new InverseCaptcha
    case 2 => new SpreadsheetChecksum
    case 3 => new SpiralMemory
    case 4 => new HighEntropyPassphrases
    case 5 => new TrampolineMaze
    case 6 => new MemoryReallocation
    case 7 => new RecursiveCircus
  }
  println(
   s"""
      |Part 1: ${solver.part1(input)}
      |Part 2: ${solver.part2(input)}
    """.stripMargin)
}
