package advent

import scala.io.Source

object Day6 extends App {
  val filename = "day6.in"

  val partOne = for {
    line <- Source.fromFile(filename).mkString.split("\n\n")
  } yield line.replace("\n", "").toSet.size

  val partTwo = for {
    line <- Source.fromFile(filename).mkString.split("\n\n")
    q = line.split("\n")
    a = q.reduce( _ intersect _)
    // a = q.tail.foldLeft(q.head)(_ intersect _)
  } yield a.length


  println(partOne.reduce(_ + _))
  println(partTwo.sum)
}
