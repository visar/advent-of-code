package advent

import scala.io.Source

object Day2 extends App {
  val filename = "day2.in"
  val pattern  = """(\d+)-(\d+) ([a-z]): (.*)""".r

  val partOne = for {
    line                              <- Source.fromFile(filename).getLines
    min :: max :: ch :: password :: _ <- pattern.unapplySeq(line)
    val a: Int     = min.toInt
    val b: Int     = max.toInt
    val c: Char    = ch.head
    val count: Int = password.count(_ == c)
    if (a <= count && count <= b)
  } yield password

  println(partOne.length)

  val partTwo = for {
    line                          <- Source.fromFile(filename).getLines
    i :: j :: ch :: password :: _ <- pattern.unapplySeq(line)
    val firstIndex  = i.toInt
    val secondIndex = j.toInt
    val c           = ch.head
    if (password(firstIndex - 1) != password(secondIndex - 1) &&
      (password(firstIndex - 1) == c || password(secondIndex - 1) == c))
  } yield password

  println(partTwo.length)
}
