package solvers

trait Solver[T] {
  def part1(input: String): T
  def part2(input: String): T
}
