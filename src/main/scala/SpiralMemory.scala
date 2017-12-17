import collection.immutable.Map

class SpiralMemory extends Solver {
  trait Direction {
    def nextDirection: Direction
    def move(position: Position): Position
  }

  object Up extends Direction {
    def nextDirection = Left
    def move(p: Position) = Position(p.x, p.y + 1)
  }

  object Left extends Direction {
    def nextDirection = Down
    def move(p: Position) = Position(p.x - 1, p.y)
  }

  object Down extends Direction {
    def nextDirection = Right
    def move(p: Position) = Position(p.x, p.y - 1)
  }

  object Right extends Direction {
    def nextDirection = Up
    def move(p: Position) = Position(p.x + 1, p.y)
  }

  case class Position(x: Int, y: Int) {
    def move(dir: Direction): Position = dir.move(this)
  }

  def part1(input: String): Int = {
    val location = input.toInt
    def loop(acc: Position, current: Int, layer: Int, dir: Direction): Position = {
      if (current == location) acc
      else {
        val endOfDir = dir match {
          case Up => acc.y == layer
          case Right => acc.x == layer + 1
          case Down => acc.y == -layer
          case Left => acc.x == -layer
        }

        if (endOfDir) {
          val nextLayer =
            if (dir == Right) layer + 1
            else layer
          loop(dir.nextDirection.move(acc), current + 1, nextLayer, dir.nextDirection)
        }
        else loop(acc.move(dir), current + 1, layer, dir)
      }
    }

    val pos = loop(Position(0, 0), 1, 0, Right)
    Math.abs(pos.x) + Math.abs(pos.y)
  }

  def part2(input: String): Int = {
    val target = input.toInt
    var memory = Map[Position, Int]((Position(0, 0), 1))
    def getMemVal(position: Position): Int = {
      val m = memory.withDefaultValue(0)
      m(Position(position.x, position.y + 1)) +
        m(Position(position.x, position.y - 1)) +
        m(Position(position.x + 1, position.y)) +
        m(Position(position.x + 1, position.y + 1)) +
        m(Position(position.x + 1, position.y - 1)) +
        m(Position(position.x - 1, position.y)) +
        m(Position(position.x - 1, position.y + 1)) +
        m(Position(position.x - 1, position.y - 1))
    }

    def loop(pos: Position, layer: Int, dir: Direction): Int = {
      val memVal = getMemVal(pos)
      if (memVal > target) memVal
      else {
        memory = memory.updated(pos, memVal)
        val endOfDir = dir match {
          case Up => pos.y == layer
          case Right => pos.x == layer + 1
          case Down => pos.y == -layer
          case Left => pos.x == -layer
        }

        if (endOfDir) {
          val nextLayer =
            if (dir == Right) layer + 1
            else layer
          loop(dir.nextDirection.move(pos), nextLayer, dir.nextDirection)
        }
        else loop(pos.move(dir), layer, dir)
      }
    }

    loop(Position(1, 0), 1, Up)
  }
}
