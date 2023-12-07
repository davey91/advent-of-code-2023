package com.brouwer.aoc.main.day3

import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object A extends App {

  val file = Source.fromResource("day3-1.txt")
  val symbols = "*#+$%@!^-/=&"

  private implicit class BoundSafeNumber(yAndX: (Y, X)) {
    def maybeDigit: String = {
      Try {
        allLines(yAndX._1).charAt(yAndX._2)
      } match {
        case util.Success(value) if value.isDigit => value.toString
        case _                                    => ""
      }
    }
  }

  private implicit class BoundSafeChecker(toCheck: => Boolean) {
    def safe = {
      Try {
        toCheck
      } match {
        case util.Failure(_)     => false
        case util.Success(value) => value
      }
    }
  }

  def hasAdjacentSymbol(char: Char, lineIndex: Int, charIndex: Int) = {
    val rightNeighbor = symbols.contains(allLines(lineIndex).charAt(charIndex + 1)).safe
    val rightTopNeighbor = symbols.contains(allLines(lineIndex + 1).charAt(charIndex + 1)).safe
    val rightBottomNeighbor = symbols.contains(allLines(lineIndex - 1).charAt(charIndex + 1)).safe
    val bottomNeighbor = symbols.contains(allLines(lineIndex - 1).charAt(charIndex)).safe
    val topNeighbor = symbols.contains(allLines(lineIndex + 1).charAt(charIndex)).safe
    val leftNeighbor = symbols.contains(allLines(lineIndex).charAt(charIndex - 1)).safe
    val leftTopNeighbor = symbols.contains(allLines(lineIndex + 1).charAt(charIndex - 1)).safe
    val leftBottomNeighbor = symbols.contains(allLines(lineIndex - 1).charAt(charIndex - 1)).safe

    char != '.' && (rightNeighbor | rightTopNeighbor | rightBottomNeighbor | bottomNeighbor | topNeighbor | leftNeighbor | leftTopNeighbor | leftBottomNeighbor)
  }

  val allLines = file.getLines().toSeq

  private type X = Int
  private type Y = Int
  val partMap: mutable.Map[Y, Seq[X]] = mutable.Map.empty[Y, Seq[X]]

  val r = allLines.zipWithIndex
    .map { case (line, lineIndex) =>
      line.foldLeft(Seq[Char]())((seq, char) => seq :+ char).zipWithIndex.map { case (char, charIndex) =>
        if (hasAdjacentSymbol(char, lineIndex, charIndex)) {
          partMap.addOne((lineIndex, partMap.getOrElse(lineIndex, Seq()) :+ charIndex))
        }
        char
      }
    }

  def findFullNumber(y: Y, x: X) = {
    val start = allLines(y).charAt(x)
    val leftSide = (y, x - 1).maybeDigit match {
      case value if value.isEmpty => ""
      case _                      => s"${(y, x - 2).maybeDigit}${(y, x - 1).maybeDigit}"
    }
    val rightSide = (y, x + 1).maybeDigit match {
      case value if value.isEmpty => ""
      case _                      => s"${(y, x + 1).maybeDigit}${(y, x + 2).maybeDigit}"
    }

    s"$leftSide$start$rightSide".trim.toInt
  }

  partMap.foreach {
    case (y, xs) => {
      val newXs = xs.foldLeft(Seq[X]())((seq, x) =>
        if (
          (seq.contains(x - 1) || seq.contains(x + 1) || seq
            .contains(x + 2)) || (seq.contains(x - 2) && allLines(y).charAt(x - 1) != '.' && !symbols.contains(allLines(y).charAt(x - 1)))
        ) seq
        else seq :+ x
      )
      partMap.update(y, newXs)
    }
  }

  val total = partMap.map { case (y, xs) => (y, xs.map(findFullNumber(y, _))) }
  println(total.values.flatten.sum)
}
