import org.scalatest.{FlatSpec, Matchers}
import solvers.RecursiveCircus

class RecursiveCircusSpec extends FlatSpec with Matchers {
  val solver = new RecursiveCircus

  "part1" should "match given example" in {
    assert(solver.part1("""pbga (66)
                          |xhth (57)
                          |ebii (61)
                          |havc (66)
                          |ktlj (57)
                          |fwft (72) -> ktlj, cntj, xhth
                          |qoyq (66)
                          |padx (45) -> pbga, havc, qoyq
                          |tknk (41) -> ugml, padx, fwft
                          |jptl (61)
                          |ugml (68) -> gyxo, ebii, jptl
                          |gyxo (61)
                          |cntj (57)""".stripMargin) === "tknk")
  }
}
