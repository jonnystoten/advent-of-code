class TrampolineMaze extends Solver {
  def part1(input: String) : Int = {
    val instructions = input.split('\n').map(_.toInt)
    def loop(acc: Int, current: Int): Int=
      if (current < 0 || current >= instructions.length) acc
      else {
        val jump = instructions(current)
        instructions(current) += 1
        val next = current + jump
        loop(acc + 1, next)
      }
    loop(0, 0)
  }

  def part2(input: String) : Int = {
    val instructions = input.split('\n').map(_.toInt)
    def loop(acc: Int, current: Int): Int=
      if (current < 0 || current >= instructions.length) acc
      else {
        val jump = instructions(current)
        val offset = if (jump >= 3) -1 else 1
        instructions(current) += offset
        val next = current + jump
        loop(acc + 1, next)
      }
    loop(0, 0)
  }
}
