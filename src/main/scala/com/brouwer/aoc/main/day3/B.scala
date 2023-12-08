package com.brouwer.aoc.main.day3

import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object B extends App {

  val file = Source.fromResource("day3-example.txt")
  val symbols = "*"
  val numbers = "0123456789"

  val leftTopPos = (y: Y, x: X) => (y + 1, x - 1)
  val leftPos = (y: Y, x: X) => (y, x - 1)
  val leftBottomPos = (y: Y, x: X) => (y - 1, x - 1)
  val topPos = (y: Y, x: X) => (y + 1, x)
  val bottomPos = (y: Y, x: X) => (y - 1, x)
  val rightTopPos = (y: Y, x: X) => (y + 1, x + 1)
  val rightPos = (y: Y, x: X) => (y, x + 1)
  val rightBottomPos = (y: Y, x: X) => (y - 1, x + 1)

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

  private implicit class BooleanOps(toCheck: => Boolean) {
    def andThen(checkAfter: Boolean) = {
      if (toCheck) {
        checkAfter
      } else toCheck
    }
  }

  def hasAdjacent(chars: String, yAndX: (Y, X), searchNested: Boolean = true, previousPosition: Option[(Y, X)] = None): Boolean = {
    val (currentY, currentX) = (yAndX._1, yAndX._2)
    val rightNeighbor = chars.contains(allLines(currentY).charAt(currentX + 1)).safe
    val rightTopNeighbor = chars.contains(allLines(currentY + 1).charAt(currentX + 1)).safe
    val rightBottomNeighbor = chars.contains(allLines(currentY - 1).charAt(currentX + 1)).safe
    val bottomNeighbor = chars.contains(allLines(currentY - 1).charAt(currentX)).safe
    val topNeighbor = chars.contains(allLines(currentY + 1).charAt(currentX)).safe
    val leftNeighbor = chars.contains(allLines(currentY).charAt(currentX - 1)).safe
    val leftTopNeighbor = chars.contains(allLines(currentY + 1).charAt(currentX - 1)).safe
    val leftBottomNeighbor = chars.contains(allLines(currentY - 1).charAt(currentX - 1)).safe

    if (!searchNested) {
      rightNeighbor | rightTopNeighbor | rightBottomNeighbor | bottomNeighbor | topNeighbor | leftNeighbor | leftTopNeighbor | leftBottomNeighbor
    } else {
      previousPosition
        .forall { case (y, x) => rightPos(y, x) != (currentY, currentX) }
        .andThen(rightNeighbor.andThen(hasAdjacent(numbers, rightPos(currentY, currentX), false, Some((currentY, currentX))))) ||
      previousPosition
        .forall { case (y, x) => rightTopPos(y, x) != (currentY, currentX) }
        .andThen(rightTopNeighbor.andThen(hasAdjacent(numbers, rightTopPos(currentY, currentX), false, Some((currentY, currentX))))) ||
      previousPosition
        .forall { case (y, x) => rightBottomPos(y, x) != (currentY, currentX) }
        .andThen(rightBottomNeighbor.andThen(hasAdjacent(numbers, rightBottomPos(currentY, currentX), false, Some((currentY, currentX))))) ||
      previousPosition
        .forall { case (y, x) => bottomPos(y, x) != (currentY, currentX) }
        .andThen(bottomNeighbor.andThen(hasAdjacent(numbers, bottomPos(currentY, currentX), false, Some((currentY, currentX))))) ||
      previousPosition
        .forall { case (y, x) => topPos(y, x) != (currentY, currentX) }
        .andThen(topNeighbor.andThen(hasAdjacent(numbers, topPos(currentY, currentX), false, Some((currentY, currentX))))) ||
      previousPosition
        .forall { case (y, x) => leftPos(y, x) != (currentY, currentX) }
        .andThen(leftNeighbor.andThen(hasAdjacent(numbers, leftPos(currentY, currentX), false, Some((currentY, currentX))))) ||
      previousPosition
        .forall { case (y, x) => leftTopPos(y, x) != (currentY, currentX) }
        .andThen(leftTopNeighbor.andThen(hasAdjacent(numbers, leftTopPos(currentY, currentX), false, Some((currentY, currentX))))) ||
      previousPosition
        .forall { case (y, x) => leftBottomPos(y, x) != (currentY, currentX) }
        .andThen(leftBottomNeighbor.andThen(hasAdjacent(numbers, leftBottomPos(currentY, currentX), false, Some((currentY, currentX)))))
    }
  }

  val allLines = file.getLines().toSeq

  private type X = Int
  private type Y = Int
  val partMap: mutable.Map[Y, Seq[X]] = mutable.Map.empty[Y, Seq[X]]

  val r = allLines.zipWithIndex
    .map { case (line, y) =>
      line.foldLeft(Seq[Char]())((seq, char) => seq :+ char).zipWithIndex.map { case (char, x) =>
        if (char != '.' && hasAdjacent(symbols, (y, x), true, Some((y, x)))) {
          partMap.addOne((y, partMap.getOrElse(y, Seq()) :+ x))
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

  partMap.map(println)

  val total = partMap.map { case (y, xs) => (y, xs.map(findFullNumber(y, _))) }
  total.map(println)
  println(total.values.flatten.sum)

  //todo, think of different approach maybe
}
