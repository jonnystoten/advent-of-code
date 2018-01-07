package solvers

class SpreadsheetChecksum extends Solver[Int] {
  def part1(input: String): Int = {
    def rowChecksum(row: String): Int = {
      val cols = row.split(Array[Char](' ', '\t')).map(c => c.toInt)
      def loop(smallest: Int, largest: Int, cols: Array[Int]): Int = {
        if (cols.isEmpty) largest - smallest
        else {
          val num = cols.head
          val s = if (num < smallest) num else smallest
          val l = if (num > largest) num else largest
          loop(s, l, cols.tail)
        }
      }
      loop(Int.MaxValue, Int.MinValue, cols)
    }

    val rows = input.split('\n')
    def loop(acc: Int, rows: Array[String]): Int =
      if (rows.isEmpty) acc
      else loop(acc + rowChecksum(rows.head), rows.tail)

    loop(0, rows)
  }

  def part2(input: String): Int = {
    def rowChecksum(row: String): Int = {
      val rowCols = row.split(Array[Char](' ', '\t')).map(c => c.toInt)
      def loop(cols: Array[Int]): Int = {
        if (cols.isEmpty) throw new Error("no two divide!")
        else {
          val other = rowCols
            .filter(c => c != cols.head)
            .find(c => cols.head % c == 0)
          other match {
            case None => loop(cols.tail)
            case Some(v) => cols.head / v
          }
        }
      }
      loop(rowCols)
    }

    val rows = input.split('\n')
    def loop(acc: Int, rows: Array[String]): Int =
      if (rows.isEmpty) acc
      else {
        val check = rowChecksum(rows.head)
        loop(acc + check, rows.tail)
      }

    loop(0, rows)
  }
}
