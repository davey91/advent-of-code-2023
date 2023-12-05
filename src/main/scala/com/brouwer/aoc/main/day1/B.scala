package com.brouwer.aoc.main.day1

import scala.io.Source

object B extends App {

  val file = Source.fromResource("day1-1.txt")

  val r = file
    .getLines()
    .toSeq
    .map(s => {
      def indexFrom(indexOf: String => Int): Seq[Int] =
        (Seq(
          indexOf("one"),
          indexOf("1"),
          indexOf("two"),
          indexOf("2"),
          indexOf("3"),
          indexOf("three"),
          indexOf("4"),
          indexOf("four"),
          indexOf("five"),
          indexOf("5"),
          indexOf("six"),
          indexOf("6"),
          indexOf("seven"),
          indexOf("7"),
          indexOf("eight"),
          indexOf("8"),
          indexOf("nine"),
          indexOf("9")
        ).filter(_ != -1).sorted)

      val firstIndices = indexFrom(s.indexOf)
      val lastIndices = indexFrom(s.lastIndexOf)

      def stringMatch(s: String, index: Int) = s match {
        case string if string.substring(index).startsWith("one") || string.substring(index).startsWith("1")   => "1"
        case string if string.substring(index).startsWith("two") || string.substring(index).startsWith("2")   => "2"
        case string if string.substring(index).startsWith("three") || string.substring(index).startsWith("3") => "3"
        case string if string.substring(index).startsWith("four") || string.substring(index).startsWith("4")  => "4"
        case string if string.substring(index).startsWith("five") || string.substring(index).startsWith("5")  => "5"
        case string if string.substring(index).startsWith("six") || string.substring(index).startsWith("6")   => "6"
        case string if string.substring(index).startsWith("seven") || string.substring(index).startsWith("7") => "7"
        case string if string.substring(index).startsWith("eight") || string.substring(index).startsWith("8") => "8"
        case string if string.substring(index).startsWith("nine") || string.substring(index).startsWith("9")  => "9"
      }

      val firstChar = stringMatch(s, firstIndices.head)
      val secondChar = stringMatch(s, lastIndices.last)

      (s"${firstChar}${secondChar}").toInt
    })
    .sum

  // 55686

  println(r)
}
