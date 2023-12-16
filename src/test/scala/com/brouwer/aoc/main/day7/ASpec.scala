package com.brouwer.aoc.main.day7

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import Card._
import com.brouwer.aoc.main.day7.A.rankHandsByHighCard

class ASpec extends AnyWordSpec with Matchers {

  "ranking hands by their high card" must {
    "do that by a seq of hand" in {
      val highHand = Hand(Seq(A, T, K, Two, Three), 0)
      val lowHand = Hand(Seq(A, K, Five, Two, Three), 0)
      val middleHand = Hand(Seq(A, K, T, Two, Three), 0)
      val handsUnordered = Seq(middleHand, lowHand, highHand)

      rankHandsByHighCard(handsUnordered) mustBe Seq(highHand, middleHand, lowHand)
    }
  }

}
