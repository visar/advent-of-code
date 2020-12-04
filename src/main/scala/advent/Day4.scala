package advent

import scala.io.Source

case class Passport private (
  birthYear: Option[Int],
  issueYear: Option[Int],
  expirationYear: Option[Int],
  height: Option[String],
  hairColor: Option[String],
  eyeColor: Option[String],
  passportId: Option[String],
  countryId: Option[String]
) {
  def isValid: Boolean =
    birthYear.isDefined &&
      issueYear.isDefined &&
      expirationYear.isDefined &&
      height.isDefined &&
      hairColor.isDefined &&
      eyeColor.isDefined &&
      passportId.isDefined

  def isExtraValid: Boolean =
    (1920 <= birthYear.get && birthYear.get <= 2002) &&
      (2010 <= issueYear.get && issueYear.get <= 2020) &&
      (2020 <= expirationYear.get && expirationYear.get <= 2030) && {
      height match {
        case Some(height) if height.endsWith("cm") =>
          val h = height.takeWhile(_.isDigit).toInt
          (150 <= h && h <= 193)
        case Some(height) if height.endsWith("in") =>
          val h = height.takeWhile(_.isDigit).toInt
          (59 <= h && h <= 76)
        case _ => false
      }
    } && {
      val pattern = "#[a-f0-9]{6}".r
      pattern.matches(hairColor.get)
    } && List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(eyeColor.get) && {
      val pattern = "[0-9]{9}".r
      pattern.matches(passportId.get)
    }
}

object Passport {
  def fromString(s: String): Passport = {
    val terms = s
      .split(" ")
      .map(_.split(":"))
      .flatten
      .grouped(2)
      .map { case Array(k, v) => k -> v }
      .toMap

    Passport(
      birthYear = terms.get("byr").map(_.toInt),
      issueYear = terms.get("iyr").map(_.toInt),
      expirationYear = terms.get("eyr").map(_.toInt),
      height = terms.get("hgt"),
      hairColor = terms.get("hcl"),
      eyeColor = terms.get("ecl"),
      passportId = terms.get("pid"),
      countryId = terms.get("cid")
    )
  }
}

object Day4 extends App {
  val filename = "day4.in"

  val partOne = (for {
    line <- Source.fromFile(filename).mkString.split("\n\n")
  } yield Passport.fromString(line.replace('\n', ' '))).filter(_.isValid)

  println(partOne.length)

  val partTwo = (for {
    line <- Source.fromFile(filename).mkString.split("\n\n")
  } yield Passport.fromString(line.replace('\n', ' ')))
    .filter(p => p.isValid && p.isExtraValid)

  println(partTwo.length)
}
