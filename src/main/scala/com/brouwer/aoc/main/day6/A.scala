package com.brouwer.aoc.main.day6

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait TimeAndDestinationParser extends RegexParsers {
  private val number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ {
    _.toInt
  }
  private val arbitrary: Parser[String] = """Time:|Distance:""".r ^^ {
    _.toString
  }

  def parser = arbitrary ~ rep(number) ^^ { case _ ~ numbers => numbers
  }
}

object A extends App with TimeAndDestinationParser {

  val file = Source.fromResource("day6-1.txt")

  val result = file
    .getLines()
    .toSeq
    .map(parse(parser, _).get)

  val timesAndDistances = result.head.zipWithIndex.map {case (value, index) => TimeAndDistance(value, result(1)(index))}

  val solutionA = timesAndDistances.map(_.nrOfWaysToBeatDistance).product

  println(solutionA)

}

case class TimeAndDistance(time: Long, distance: Long) {
  val calculateDistances = 0L to time map(timeToUse => timeToUse * 1L * (time - timeToUse))

  val nrOfWaysToBeatDistance = calculateDistances.count(_ > distance)
}
