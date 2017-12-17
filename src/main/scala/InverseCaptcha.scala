class InverseCaptcha extends Solver {
  def solve(findMid: (List[Int] => Int))(input: String): Int = {
    val list = input.toList.map(c => c.asDigit)
    val mid = findMid(list)
    val list2 = list.slice(mid, list.length) ++ list.slice(0, mid)

    def sum(current: Int, next: Int) =
      if (current == next) current
      else 0

    def loop(acc: Int, digits1: List[Int], digits2: List[Int]): Int =
      if (digits1.isEmpty) acc
      else loop(acc + sum(digits1.head, digits2.head), digits1.tail, digits2.tail)

    loop(0, list, list2)
  }

  val one: String => Int = solve(_ => 1)
  def part1(input: String): Int = one(input)

  val two: String => Int = solve(l => l.length / 2)
  def part2(input: String): Int = two(input)
}
