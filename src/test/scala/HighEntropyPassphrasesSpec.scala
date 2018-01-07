import org.scalatest.{FlatSpec, Matchers}
import solvers.HighEntropyPassphrases

class HighEntropyPassphrasesSpec extends FlatSpec with Matchers {
  val solver = new HighEntropyPassphrases

  "part1" should "match given examples" in {
    assert(solver.part1("""aa bb cc dd ee
                          |aa bb cc dd aa
                          |aa bb cc dd aaa""".stripMargin) === 2)
  }

  "part2" should "match given examples" in {
    assert(solver.part2("""abcde fghij
                          |abcde xyz ecdab
                          |a ab abc abd abf abj
                          |iiii oiii ooii oooi oooo
                          |oiii ioii iioi iiio""".stripMargin) === 3)
  }
}
