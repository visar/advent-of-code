package advent

import scala.io.Source

case class Row(start: Int, end: Int)
case class Column(start: Int, end: Int)

case class Seat(bsp: String) {
  def id: Int = {
    val seat: (Row, Column) = bsp.foldLeft(Row(0, 127), Column(0, 7)) {
      case ((row, column), 'F') => (Row(row.start, row.start + (row.end - row.start) / 2), column)
      case ((row, column), 'B') => (Row(row.start + (row.end - row.start) / 2 + 1, row.end), column)
      case ((row, column), 'L') => (row, Column(column.start, column.start + (column.end - column.start) / 2))
      case ((row, column), 'R') => (row, Column(column.start + (column.end - column.start) / 2 + 1, column.end))
      case ((row, column), _)   => (row, column)
    }
    val row: Row       = seat._1
    val column: Column = seat._2

    8 * row.start + column.start
  }
}

object Day5 extends App {
  val filename = "day5.in"

  val partOne = (for {
    line <- Source.fromFile(filename).getLines
  } yield Seat(line).id).toList

  println(partOne.max)
  println((partOne.min to partOne.max).filterNot(partOne.toSet))
}
