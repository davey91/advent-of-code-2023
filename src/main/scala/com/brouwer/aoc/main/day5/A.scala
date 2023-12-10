package com.brouwer.aoc.main.day5

import com.brouwer.aoc.main.day5.SeedsAndMapping._

import scala.collection.immutable.NumericRange
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

trait SeedsParser extends RegexParsers {
  private val number: Parser[Long] = """(0|[1-9]\d*)""".r ^^ { _.toLong }
  def seedsPrefix: Parser[String] = """[seeds:]+""".r ^^ { _.toString }

  def seeds = seedsPrefix ~ rep(number) ^^ { case _ ~ seeds => Seeds(seeds) }
}

trait DestinationSourceParser extends RegexParsers {
  private val number: Parser[Long] = """(0|[1-9]\d*)""".r ^^ { _.toLong }

  def destinationSourceRange(seedsAndMapping: SeedsAndMapping) = number ~ number ~ number ^^ { case destination ~ source ~ range =>
    seedsAndMapping.update(DestinationAndSourceRange(destination, source, range))
  }
}

object A extends App with SeedsParser with DestinationSourceParser {

  val file = Source.fromResource("day5-1.txt")

  val seedsAndMapping = file
    .getLines()
    .toSeq
    .foldLeft(SeedsAndMapping.empty) { case (seedsAndMapping, line) =>
      line match {
        case line if line.isBlank                                    => seedsAndMapping
        case line if line.startsWith("seeds:")                       => SeedsAndMapping(parse(seeds, line).get, Map.empty, Empty)
        case line if line.startsWith("seed-to-soil map:")            => seedsAndMapping.copy(currentKey = SeedToSoil)
        case line if line.startsWith("soil-to-fertilizer map:")      => seedsAndMapping.copy(currentKey = SoilToFertilizer)
        case line if line.startsWith("fertilizer-to-water map:")     => seedsAndMapping.copy(currentKey = FertilizerToWater)
        case line if line.startsWith("water-to-light map:")          => seedsAndMapping.copy(currentKey = WaterToLight)
        case line if line.startsWith("light-to-temperature map:")    => seedsAndMapping.copy(currentKey = LightToTemperature)
        case line if line.startsWith("temperature-to-humidity map:") => seedsAndMapping.copy(currentKey = TemperatureToHumidity)
        case line if line.startsWith("humidity-to-location map:")    => seedsAndMapping.copy(currentKey = HumidityToLocation)
        case line                                                    => parse(destinationSourceRange(seedsAndMapping), line).get
      }
    }

  val solutionA = seedsAndMapping.findLocations.min
  println(solutionA)

}

object SeedsAndMapping {
  def empty = SeedsAndMapping(Seeds(Nil), Map.empty, Empty)
  sealed trait MapType
  case object Empty extends MapType
  case object SeedToSoil extends MapType
  case object SoilToFertilizer extends MapType
  case object FertilizerToWater extends MapType
  case object WaterToLight extends MapType
  case object LightToTemperature extends MapType
  case object TemperatureToHumidity extends MapType
  case object HumidityToLocation extends MapType

  val All =
    Seq(SeedToSoil, SoilToFertilizer, FertilizerToWater, WaterToLight, LightToTemperature, TemperatureToHumidity, HumidityToLocation)
}

case class Seeds(seeds: Seq[Long])
object DestinationAndSourceRange {
  def apply(destination: Long, source: Long, range: Long): DestinationAndSourceRange = {
    DestinationAndSourceRange(destination until destination + range, source until source + range)
  }
}
case class DestinationAndSourceRange(destinationRange: NumericRange[Long], sourceRange: NumericRange[Long]) {
  def findDestination(from: Long) = {
    sourceRange.find(_ == from).map(_ - sourceRange.start).map(index => destinationRange(index.toInt)).get
  }
}

case class SeedsAndMapping(seeds: Seeds, map: Map[MapType, Seq[DestinationAndSourceRange]], currentKey: MapType) {
  def update(destinationAndSourceRange: DestinationAndSourceRange) =
    copy(map = map.updated(currentKey, map.getOrElse(currentKey, Nil) :+ destinationAndSourceRange))

  def findInRange(mapType: MapType, toFind: Long) = {
    map(mapType).find(_.sourceRange.contains(toFind)).map(_.findDestination(toFind)).getOrElse(toFind)
  }

  def findLocations = {
    seeds.seeds.map(seed => {
      All.foldLeft(seed)((toFind, mapType) => findInRange(mapType, toFind))
    })
  }
}
