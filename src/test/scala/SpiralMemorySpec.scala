import org.scalatest.{FlatSpec, Matchers}
import solvers.SpiralMemory

class SpiralMemorySpec extends FlatSpec with Matchers {
  val solver = new SpiralMemory

  "part1" should "match given examples" in {
    assert(solver.part1("1") === 0)
    assert(solver.part1("12") === 3)
    assert(solver.part1("23") === 2)
    assert(solver.part1("1024") === 31)
  }
}
