package advent

import scala.io.Source

case class Position(x: Int, y: Int, isTree: Boolean)

object Day3 extends App {
  def trees(filename: String, row: Int, col: Int): Long = {
    val c = Source.fromFile(filename).getLines.toList(0).length

    (for {
      line       <- Source.fromFile(filename).getLines.zipWithIndex
      indexedRow <- line._1.zipWithIndex
      row    = line._2
      column = indexedRow._2
      ch     = indexedRow._1
    } yield Position(row, column, ch == '#'))
      .foldLeft(0, (0, 0)) {
        case ((acc, (i, j)), p) if p.x == i && p.y == j && p.isTree =>
          (acc + 1, (i + row, (j + col) % c))
        case ((acc, (i, j)), p) if p.x == i && p.y == j =>
          (acc, (i + row, (j + col) % c))
        case ((acc, (i, j)), p) =>
          (acc, (i, j))
      }
      ._1
  }

  println(trees("day3.in", 1, 3))
  println(
    trees("day3.in", 1, 1) *
      trees("day3.in", 1, 3) *
      trees("day3.in", 1, 5) *
      trees("day3.in", 1, 7) *
      trees("day3.in", 2, 1)
  )
}
