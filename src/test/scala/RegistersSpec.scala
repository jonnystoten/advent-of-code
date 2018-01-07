import org.scalatest.{FlatSpec, Matchers}
import solvers.Registers

class RegistersSpec extends FlatSpec with Matchers {
  val solver = new Registers

  "part1" should "match given example" in {
    assert(solver.part1("""b inc 5 if a > 1
                          |a inc 1 if b < 5
                          |c dec -10 if a >= 1
                          |c inc -20 if c == 10""".stripMargin) === 1)
  }

  "part2" should "match given example" in {
    assert(solver.part2("""b inc 5 if a > 1
                          |a inc 1 if b < 5
                          |c dec -10 if a >= 1
                          |c inc -20 if c == 10""".stripMargin) === 10)
  }
}
