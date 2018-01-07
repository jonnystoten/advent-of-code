package solvers

class RecursiveCircus extends Solver[String] {
  trait Node {
    val name: String
    val weight: Int
    def children: List[String]
  }

  case class Leaf(name: String, weight: Int) extends Node {
    override def children: List[String] = List()
  }
  case class NonLeaf(name: String, weight: Int, children: List[String]) extends Node

  def part1(input: String): String = {
    val lines = input.split('\n').toList
    val leafRegex = """([a-z]+) \((\d+)\)""".r
    val nonLeafRegex = (leafRegex + """ -> (.*)""").r
    val nodes: List[Node] = lines.map {
      case leafRegex(name, weight) => Leaf(name, weight.toInt)
      case nonLeafRegex(name, weight, children) => NonLeaf(name, weight.toInt, children.split(", ").toList)
    }

    def findRoot(done: List[Node], todo: List[Node]): Node = {
      if (todo.length == 1) todo.head
      else if (todo.head.children.forall(p => done.exists(d => d.name == p))) findRoot(todo.head :: done, todo.tail)
      else findRoot(done, todo.tail :+ todo.head)
    }

    val root = findRoot(List(), nodes)
    root.name
  }

  def part2(input: String): String = ???
}
