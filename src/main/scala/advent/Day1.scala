package advent

import scala.io.Source

object Day1 extends App {
  val filename = "day1.in"
  val lines    = Source.fromFile(filename).getLines.toList.map(_.toInt).sorted

  println(lines.combinations(2).filter(_.sum == 2020).toList.head.product)
  println(lines.combinations(3).filter(_.sum == 2020).toList.head.product)
}
