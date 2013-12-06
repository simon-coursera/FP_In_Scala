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
    val constParenthesesPairs = List('(', ')')

    def matchParenthese(chars: List[Char], head: Int): Int = {
      if (chars(head) != constParenthesesPairs(0)) -1
      else {
        var iHead: Int = head + 1
        var isFound = false

        while (iHead < chars.length && !isFound) {
          if (chars(iHead) == constParenthesesPairs(0)) { //Find another parenthese
            iHead = matchParenthese(chars, iHead)
          } else {
            if (chars(iHead) == constParenthesesPairs(1)) isFound = true

            iHead = iHead + 1
          }
        }

        if (isFound) iHead else -1
      }
    }

    val parentheseChars = chars.filter(x => x == '(' || x == ')')
    if (parentheseChars.isEmpty) true
	else {
		var iHead = 0
		while (iHead < parentheseChars.length && iHead >= 0) {
		  iHead = matchParenthese(parentheseChars, iHead)
		}

		iHead != -1
	}
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var allCombinations = Set.empty[List[Int]]

    def findChange(money: Int, coins: List[Int], combinations: List[Int]): Unit = {
      val fitCoins = coins.filter(c => c <= money).sortWith(_ >= _)

      fitCoins.foreach { x =>
        {
          if (x == money) {
            val foundCombinations = combinations ::: List(x)
            allCombinations += foundCombinations
            //print("found ")
            //println(foundCombinations)
          } else {
            findChange(money - x, fitCoins.filter(c => x <= c ), combinations ::: List(x))
          }
        }
      }
    }

    findChange(money, coins, List())
    allCombinations.size
  }   
}
