import org.scalatest.{FlatSpec, Matchers}

class InverseCaptchaSuite extends FlatSpec with Matchers {
  val solver = new InverseCaptcha()
  "part1" should "match given examples" in {
    assert(solver.part1("1122") === 3)
    assert(solver.part1("1111") === 4)
    assert(solver.part1("1234") === 0)
    assert(solver.part1("91212129") === 9)
  }

  "part2" should "match given examples" in {
    assert(solver.part2("1212") === 6)
    assert(solver.part2("1221") === 0)
    assert(solver.part2("123425") === 4)
    assert(solver.part2("123123") === 12)
    assert(solver.part2("12131415") === 4)
  }
}
