package solvers

class Registers extends Solver[Int] {
  case class Instruction(register: String, amount: Int, condition: Condition)
  case class Condition(register: String, operator: String, amount: Int)

  def process(input: String): (Int, Int) = {
    val codes = input.split('\n').map(s => s.trim).filter(s => s != "")
    val regex = """([a-z]+) (inc|dec) (-?\d+) if ([a-z]+) (.*) (-?\d+)""".r

    var instructions = codes.map {
      case regex(register, op, amount, condReg, condOp, condAmount) => {
        val x = if (op == "inc") amount.toInt else -amount.toInt
        Instruction(register, x, Condition(condReg, condOp, condAmount.toInt))
      }
    }

    var registers = Map[String, Int]()
    var max = Int.MinValue

    def conditionIsTrue(condition: Condition): Boolean = {
      if (!registers.contains(condition.register))
        registers = registers.updated(condition.register, 0)

      val value = registers(condition.register)

      val amount = condition.amount
      condition.operator match {
        case ">" => value > amount
        case "<" => value < amount
        case ">=" => value >= amount
        case "<=" => value <= amount
        case "==" => value == amount
        case "!=" => value != amount
      }
    }

    instructions.foreach(i => {
      if (!registers.contains(i.register))
        registers = registers.updated(i.register, 0)
      if (conditionIsTrue(i.condition)) {
        val value = registers(i.register)
        val newValue = value + i.amount
        max = if (newValue > max) newValue else max
        registers = registers.updated(i.register, value + i.amount)
      }
    })

    (registers.maxBy(kv => kv._2)._2, max)
  }

  def part1(input: String): Int = {
    process(input)._1
  }

  def part2(input: String): Int = {
    process(input)._2
  }
}
