package com.brouwer.aoc.main.day6

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait TimeAndDestinationUntrimmedParser extends RegexParsers {
  private val trimmedNumber: Parser[Long] = """.*""".r ^^ { case untrimmed => untrimmed.replace(" ", "").trim.toLong }
  private val arbitrary: Parser[String] = """Time:|Distance:""".r ^^ {
    _.toString
  }

  def parser = arbitrary ~ trimmedNumber ^^ { case _ ~ number => number
  }
}

object B extends App with TimeAndDestinationUntrimmedParser {

  val file = Source.fromResource("day6-1.txt")

  val result = file
    .getLines()
    .toSeq
    .map(parse(parser, _).get)

  val timesAndDistances = TimeAndDistance(result.head, result(1))

  val solution = timesAndDistances.nrOfWaysToBeatDistance

  println(solution)

}