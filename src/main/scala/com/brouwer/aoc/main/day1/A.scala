package com.brouwer.aoc.main.day1

import scala.io.Source

object A extends App {

  val file = Source.fromResource("day1-1.txt")

  val r = file
    .getLines()
    .toSeq
    .map(_.filter(_.isDigit))
    .map {
      case s if s.length == 1 =>
        s"${s.head}${s.head}".toInt
      case s =>
        s"${s.head}${s.last}".toInt
    }.sum

  println(r)
}
