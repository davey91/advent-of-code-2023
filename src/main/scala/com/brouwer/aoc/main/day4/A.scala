package com.brouwer.aoc.main.day4

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class Card(number: Int, winningNumbers: List[Int], cardNumbers: List[Int]) {
  override def toString = s"$number, winningNumbers: $winningNumbers, cardNumbers: $cardNumbers"
  val totalWin: List[Int] = winningNumbers.intersect(cardNumbers)
  val totalPoints = totalWin match {
    case Nil => 0
    case notEmpty => notEmpty.tail.foldLeft(1)((total, _) => total * 2)
  }
}

trait SimpleParser extends RegexParsers {
  def card: Parser[String] = """[Card]+""".r ^^ { _.toString }
  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def semicolon: Parser[String] = """:""".r ^^ { _.toString }
  def winningNumbersSeperator: Parser[String] = """\|""".r ^^ { _.toString }
  def cardWithGames: Parser[Card] = card ~ number ~ semicolon ~ rep(number) ~ winningNumbersSeperator ~ rep(number) ^^ { case cd ~ nr ~ _ ~ winningNumbers ~ _ ~ cardNumbers => Card(nr, winningNumbers, cardNumbers) }
}

object A extends App with SimpleParser {

  val file = Source.fromResource("day4-1.txt")

  val r = file
    .getLines()
    .toSeq
    .map(parse(cardWithGames, _))
    .map {
      case Success(matched, _) => matched.totalPoints
      case Failure(_, _) => 0
      case Error(_, _) => 0
    }.sum

  println(r)

}
