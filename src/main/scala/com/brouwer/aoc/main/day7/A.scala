package com.brouwer.aoc.main.day7

import com.brouwer.aoc.main.day7.Card.stringToCard
import com.brouwer.aoc.main.day7.TypeOfHand._

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait CardParser extends RegexParsers {
  private val card: Parser[String] = """([A-Z0-9]){1}""".r ^^ {
    _.trim
  }

  private val bid: Parser[Int] = """(\d*)""".r ^^ { bid => bid.toInt }

  def parser = card ~ card ~ card ~ card ~ card ~ bid ^^ { case card1 ~ card2 ~ card3 ~ card4 ~ card5 ~ bid =>
    Hand(Seq(card1, card2, card3, card4, card5).map(stringToCard), bid)
  }
}

object A extends App with CardParser {

  val file = Source.fromResource("day7-example.txt")

  val result = file
    .getLines()
    .toSeq
    .map(parse(parser, _).get)

  // println(result)

  /*  println(result.groupBy(_.typeOfHand).map{case (typeOfHand, hands) => {
    hands.zipWithIndex.map {case (hand, i) =>
      val rank = hands.zipWithIndex.foldLeft(Option.empty[Int]){case (position, (hand, index)) => position match {
        case defined if defined.isDefined => defined
        case None => hand.compareWith(hands(index)) match {
          case true => Some(index)
          case _ => None
        }
      }}.getOrElse(hands.siz)
      println(rank)
      RankedHand(hand, typeOfHand, rank)
    }
  }})*/
  /*   ordered.zipWithIndex.foldLeft(Seq.empty[Hand]){case (orderedHands, (orderedHand, index)) =>
     if(orderedHand == hand) orderedHands
     else if(hand.strongerThan(orderedHand)) {

     }*/
  def rankHandsByHighCard(hands: Seq[Hand]): Seq[Hand] = {
    var ordered: Seq[Hand] = Seq(hands.head)
    hands.foreach(hand => {
      var handIndex = Option.empty[Int]
      ordered.zipWithIndex.foreach(orderedHandAndIndex => {
        if (orderedHandAndIndex._1 != hand && !orderedHandAndIndex._1.strongerThan(hand)) {
          handIndex = Some(orderedHandAndIndex._2)
        }
      })
      if(handIndex.isDefined) {
        val leftAndRight = ordered.splitAt(handIndex.get)
        ordered = leftAndRight._1 ++ Seq(hand) ++ leftAndRight._2
      }
    })
    ordered
  }
}

case class RankedHand(hand: Hand, typeOfHand: TypeOfHand, rank: Int)

case class Hand(cards: Seq[Card], bid: Int) {
  val typeOfHand: TypeOfHand = {
    val countedCards = cards.groupBy(_.name)
    countedCards.size match {
      case 1                                             => FiveOfAKind
      case 2 if countedCards.values.map(_.size).max == 4 => FourOfAKind
      case 2                                             => FullHouse
      case 3 if countedCards.values.map(_.size).max == 3 => ThreeOfAKind
      case 3                                             => TwoPair
      case 4                                             => OnePair
      case 5                                             => HighCard
      case longerSet                                     => throw new Error(s"Unexpected amount of different cards ${longerSet}")
    }
  }

  def strongerThan(hand: Hand): Boolean = {
    // println(hand)
    // println(cards)
    val result = cards.zipWithIndex
      .foldLeft(Option.empty[Boolean]) { case (isStronger, (card, i)) =>
        if (isStronger.isDefined) isStronger
        else {
          if (card.value == hand.cards(i).value) None
          else if (card.value > hand.cards(i).value) Some(true)
          else Some(false)
        }
      }
      .getOrElse(false)
    println(result)
    result
  }

  override def toString: String = s"$typeOfHand: ${cards.map(_.name).mkString}"
}
sealed abstract class TypeOfHand(val value: Int)
object TypeOfHand {
  val allTypesOfHands = Seq(HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind)

  case object HighCard extends TypeOfHand(0)
  case object OnePair extends TypeOfHand(1)
  case object TwoPair extends TypeOfHand(2)
  case object ThreeOfAKind extends TypeOfHand(3)
  case object FullHouse extends TypeOfHand(4)
  case object FourOfAKind extends TypeOfHand(5)
  case object FiveOfAKind extends TypeOfHand(6)
}

sealed abstract class Card(val value: Int, val name: String)
object Card {
  val allCards: Seq[Card] = Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, T, J, Q, K, A)
  def stringToCard(string: String): Card = Card.allCards.find(card => card.name == string).get

  case object Two extends Card(0, "2")
  case object Three extends Card(1, "3")
  case object Four extends Card(2, "4")
  case object Five extends Card(3, "5")
  case object Six extends Card(4, "6")
  case object Seven extends Card(5, "7")
  case object Eight extends Card(6, "8")
  case object Nine extends Card(7, "9")
  case object T extends Card(8, "T")
  case object J extends Card(9, "J")
  case object Q extends Card(10, "Q")
  case object K extends Card(11, "K")
  case object A extends Card(12, "A")
}
