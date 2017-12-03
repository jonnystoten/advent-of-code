object Direction extends Enumeration {
  type Direction = Value
  val Up, Down, Left, Right = Value
}

case class Position(x: Int, y: Int) {
  def move(dir: Direction.Value): Position =
    dir match {
      case Direction.Up => Position(x, y + 1)
      case Direction.Right => Position(x + 1, y)
      case Direction.Down => Position(x, y - 1)
      case Direction.Left => Position(x - 1, y)
    }
}

def nextDirection(dir: Direction.Value): Direction.Value =
  dir match {
    case Direction.Up => Direction.Left
    case Direction.Left => Direction.Down
    case Direction.Down => Direction.Right
    case Direction.Right => Direction.Up
  }

def part1(location: Int): Int = {
  def loop(acc: Position, current: Int, layer: Int, dir: Direction.Value): Position = {
    if (current == location) acc
    else {
      val endOfDir = dir match {
        case Direction.Up =>
          acc.y == layer
        case Direction.Right =>
          acc.x == layer + 1
        case Direction.Down =>
          acc.y == -layer
        case Direction.Left =>
          acc.x == -layer
      }

      if (endOfDir) {
        val nextLayer =
          if (dir == Direction.Right) layer + 1
          else layer
        loop(acc.move(nextDirection(dir)), current + 1, nextLayer, nextDirection(dir))
      }
      else loop(acc.move(dir), current + 1, layer, dir)
    }
  }

  val pos = loop(Position(0, 0), 1, 0, Direction.Right)
  Math.abs(pos.x) + Math.abs(pos.y)
}

part1(1)
part1(12)
part1(23)
part1(1024)
part1(312051)

import collection.immutable.Map

def part2(target: Int): Int = {
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

  def loop(pos: Position, layer: Int, dir: Direction.Value): Int = {
    val memVal = getMemVal(pos)
    if (memVal > target) memVal
    else {
      memory = memory.updated(pos, memVal)
      val endOfDir = dir match {
        case Direction.Up =>
          pos.y == layer
        case Direction.Right =>
          pos.x == layer + 1
        case Direction.Down =>
          pos.y == -layer
        case Direction.Left =>
          pos.x == -layer
      }

      if (endOfDir) {
        val nextLayer =
          if (dir == Direction.Right) layer + 1
          else layer
        loop(pos.move(nextDirection(dir)), nextLayer, nextDirection(dir))
      }
      else loop(pos.move(dir), layer, dir)
    }
  }

  loop(Position(1, 0), 1, Direction.Up)
}

part2(312051)