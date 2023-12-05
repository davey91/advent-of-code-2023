package com.brouwer.aoc.main.day2

import scala.io.Source
import scala.util.matching.Regex

//12 red cubes, 13 green cubes, and 14 blue cubes
object A extends App {

  val file = Source.fromResource("day2-1.txt")

  val gameRegex: Regex = """Game (\d{0,3}): (.*)""".r
  val colorRegex: Regex = """(\d{0,2}) (.*)""".r

  val r = file
    .getLines()
    .toSeq
    .map { case gameRegex(gameId, gameCubes) =>
      val cubes = gameCubes
        .split(";")
        .map(cubes =>
          cubes
            .split(",")
            .map(_.trim)
            .foldLeft(Cubes())((cubes, cubeToAdd) =>
              cubeToAdd match {
                case colorRegex(n, color) =>
                  color match {
                    case "blue"  => cubes.copy(blue = n.toInt)
                    case "red"   => cubes.copy(red = n.toInt)
                    case "green" => cubes.copy(green = n.toInt)
                  }
              }
            )
        )
      Game(gameId, cubes.zipWithIndex.map { case (cubes, index) => index -> cubes }.toMap)
    }

  val solutionPart1 = r.filter(_.canBePlayed(Cubes(red = 12, green = 13, blue = 14))).map(_.id.toInt).sum
  println(solutionPart1)
  val solutionPart2 = r.map(_.leastCubesNeededPower).sum
  println(solutionPart2)
}

case class Game(id: String, sets: Map[Int, Cubes]) {
  def canBePlayed(toCheck: Cubes): Boolean = sets.values.forall(_.canBePlayed(toCheck))
  def leastCubesNeededPower: Int = {
    val red = sets.values.map(_.red).max
    val green = sets.values.map(_.green).max
    val blue = sets.values.map(_.blue).max
    red * blue * green
  }
}
case class Cubes(red: Int = 0, green: Int = 0, blue: Int = 0) {
  def canBePlayed(toCheck: Cubes): Boolean = red <= toCheck.red && green <= toCheck.green && blue <= toCheck.blue
}
