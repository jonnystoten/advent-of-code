import scala.io.Source

object Main extends App {
  val day = args(0).toInt
  var input = Source.fromFile(s"input/$day.txt").mkString.trim
  val solver = day match {
    case 1 => new InverseCaptcha()
  }
  println(
   s"""
      |Part 1: ${solver.part1(input)}
      |Part 2: ${solver.part2(input)}
    """.stripMargin)
}
