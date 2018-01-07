import org.scalatest.{FlatSpec, Matchers}
import solvers.TrampolineMaze

class TrampolineMazeSpec extends FlatSpec with Matchers {
  val solver = new TrampolineMaze

  "part1" should "match given example" in {
    assert(solver.part1("""0
                          |3
                          |0
                          |1
                          |-3
                          |""".stripMargin) === 5)
  }

  "part2" should "match given example" in {
    assert(solver.part2("""0
                          |3
                          |0
                          |1
                          |-3
                          |""".stripMargin) === 10)
  }
}
