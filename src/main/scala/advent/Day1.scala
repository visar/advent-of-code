package advent

import scala.io.Source

object Day1 extends App {
  val filename = "day1.in"
  val lines = Source.fromFile(filename).getLines.toList.map(_.toInt).sorted

  val partOne = (for {
    x <- lines
    if x < 2020 - x && lines.contains(2020 - x)
  } yield x * (2020 - x)).head

  println(partOne)

  val partTwo = (for {
    x <- lines
    y <- lines
    z <- lines
    if x < y && y < z && x + y + z == 2020
  } yield x * y * z).head

  println(partTwo)
}
