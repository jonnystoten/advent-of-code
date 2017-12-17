import org.scalatest.{FlatSpec, Matchers}

class SpreadsheetChecksumSpec extends FlatSpec with Matchers {
  val solver = new SpreadsheetChecksum

  "part1" should "match given example" in {
    assert(solver.part1("""5 1 9 5
                          |7 5 3
                          |2 4 6 8""".stripMargin) === 18)
  }

  "part2" should "match given example" in {
    assert(solver.part2("""5 9 2 8
                          |9 4 7 3
                          |3 8 6 5""".stripMargin) === 9)
  }
}
