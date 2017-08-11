import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by pratzlif on 8/10/17.
  */
object TestCommands {
  /*
  Coin flip methods
   */
  def flipNCoinsWithProbability(n: Int, prob: Double): Int = {
    if(prob < 0 || prob > 1) {
      throw new IllegalArgumentException("Please enter a probability between 0 and 1")
    } else if (n > 1000000) {
      throw new IllegalArgumentException("Capped at 1000000\n" + flipNCoinsWithProbability(1000000, prob))
    } else if (n < 1) {
      throw new IllegalArgumentException("Why would you flip less than one coin?")
    }
    else {
      @tailrec def recursiveFlip(remaining: Int, numHeads: Int): Int = {
        if(remaining <= 0) {
          numHeads
        } else {
          val success = if(Random.nextDouble() < prob) 1 else 0
          recursiveFlip(remaining-1, numHeads+success)
        }
      }
      recursiveFlip(n, 0)
    }
  }
  def flipBiasedCoin(prob: Double): Int = {
    flipNCoinsWithProbability(1, prob)
  }
  def flipNCoins(n: Int): Int = {
    flipNCoinsWithProbability(n, 0.5)
  }
  def flipCoin: Int = {
    flipNCoins(1)
  }

  /*
  Dice methods
   */
  def rollNDice(sides: Int, n: Int): List[Int] = {
    if(sides < 2) {
      throw new IllegalArgumentException("Please choose at least 2 sides.")
    } else if(n < 1) {
      throw new IllegalArgumentException("Great, I'm done.")
    }
    else {
      @tailrec
      def recursiveRoll(results: List[Int], times: Int): List[Int] = {
        if(times <= 0) {
          results
        }
        else {
          val thisRoll = Random.nextInt(sides) + 1
          recursiveRoll(thisRoll::results, times-1)
        }
      }
      recursiveRoll(List(), n)
    }
  }
  def rollDie(sides: Int): List[Int] = {
    rollNDice(sides, 1)
  }
  def roll: List[Int] = {
    rollNDice(6, 1)
  }

  /*
  Card methods
   */
  val ranks = Array(2, 3, 4, 5, 6, 7, 8, 9, 10, "Jack", "Queen", "King", "Ace")
  val suits = Array("Hearts", "Diamonds", "Spades", "Clubs")
  var cards:List[(Any, String)] = ranks.flatMap(elem => suits.map(suit => (elem, suit))).toList
  def shuffleDeck[A](deck: List[A]): List[A] = util.Random.shuffle(deck)
  def drawFromList[A](num: Int, deck: List[A]): List[(A)] = {
    if(num > deck.length) {
      throw new IllegalArgumentException("Not enough cards left!")
    }
    else {
      def recursiveDraw(hand: List[A], deck: List[A], toDraw: Int): List[A] = {
        if(toDraw <= 0) {
          hand
        }
        else {
          recursiveDraw(deck.head::hand, deck.tail, toDraw-1)
        }
      }
      recursiveDraw(List[A](), shuffleDeck(deck), num)
    }
  }
  def drawCard: List[(Any, String)] = {
    drawFromList(1, cards)
  }

  /*
  Monty Hall methods
   */
  def montyHall(options: Int, choice: Int, switch: Boolean): Boolean = {
    val result = Random.nextInt(options) + 1
    !((result==choice) == switch)
  }
  def montyHallRepeat(times: Int, options: Int, switch: Boolean): Int = {
    @tailrec
    def recurse(times: Int, successes: Int): Int = {
      if(times > 0) {
        val result = if (montyHall(options, Random.nextInt(options) + 1, switch)) 1 else 0
        recurse(times-1, successes+result)
      }
      else {
        successes
      }
    }
    recurse(times, 0)
    //println(s"You won ${recurse(times, 0)} out of $times games. Switch: $switch")
  }
  def mHSwapTest(wins: Int): Int = {
    @tailrec
    def recurseGames(goal: Int, wins: Int): Int = {
      if(montyHallRepeat(100, 3, false) > wins) {
        goal+1
      } else {
        recurseGames(goal+1, wins)
      }
    }
    recurseGames(1, wins)
  }
}