package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || c > r) 0 else if (c == 0 && r == 0) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */


  def balance(chars: List[Char]): Boolean = {

    def reduceLeftParenthese(parentheseChars: List[Char]): (Boolean, List[Char]) = {
      if (parentheseChars.isEmpty) (false, List.empty)
      else {
        if (parentheseChars.head != '(') (false, List.empty)
        else reduceRightParenthese(parentheseChars.tail)
      }
    }

    def reduceRightParenthese(parentheseChars: List[Char]): (Boolean, List[Char]) = {
      if (parentheseChars.isEmpty) (false, parentheseChars)
      else {
        if (parentheseChars.head == '(') {
          val (success, remainings) = reduceLeftParenthese(parentheseChars)
          if (!success) (false, List.empty)
          else reduceRightParenthese(remainings)
        } else (true, parentheseChars.tail)
      }
    }

    val parentheseChars = chars.filter(x => x == '(' || x == ')')
    if (parentheseChars.isEmpty) true
    else {
      val (success, remainings) = reduceLeftParenthese(parentheseChars)

      if (!success) false
      else {
        if (remainings.isEmpty) true
        else balance(remainings)
      }
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def findChange(money: Int, coins: List[Int], matchingCoins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else {
      	val restCount = findChange(money, coins.tail, matchingCoins)	//Find the rest combinations without head coin
      	
        if (coins.head == money) 1 + restCount
        else if (coins.head < money) findChange(money - coins.head, coins, matchingCoins ::: List(coins.head)) + restCount
        else restCount
      }
    }

    findChange(money, coins.filter(c => c <= money).sortWith(_ >= _), List())
  }
}
