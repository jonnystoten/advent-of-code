import org.scalatest.{FlatSpec, Matchers}
import solvers.MemoryReallocation

class MemoryAllocationSpec extends FlatSpec with Matchers {
  val solver = new MemoryReallocation

  "part1" should "match given example" in {
    assert(solver.part1("0\t2\t7\t0") === 5)
  }

  "part2" should "match given example" in {
    assert(solver.part2("0\t2\t7\t0") === 4)
  }
}
