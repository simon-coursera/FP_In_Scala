package recfun

object test1 {

  def countChange(money: Int, coins: List[Int]): Int = {
    var allCombinations = Set.empty[List[Int]]

    def findChange(money: Int, coins: List[Int], combinations: List[Int]): Unit = {
      val fitCoins = coins.filter(c => c <= money).sortWith(_ >= _)

      fitCoins.foreach { x =>
        {
          //val remainingMoney = money % x

          //if (remainingMoney == 0) {
          if (x == money) {
            val foundCombinations = combinations ::: List(x)
            allCombinations += foundCombinations
            //print("found ")
            //println(foundCombinations)
          } else {
            findChange(money - x, fitCoins.filter(c => x <= c), combinations ::: List(x))
          }
        }
      }
    }

    findChange(money, coins, List())
    allCombinations.size
  }                                               //> countChange: (money: Int, coins: List[Int])Int

  def countChange_new(money: Int, coins: List[Int]): Int = {
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
  }                                               //> countChange_new: (money: Int, coins: List[Int])Int

  val coins = List(5, 10, 20, 50, 100, 200, 500)  //> coins  : List[Int] = List(5, 10, 20, 50, 100, 200, 500)
  val coins1 = List(1, 2, 3)                      //> coins1  : List[Int] = List(1, 2, 3)

  countChange(300, coins)                         //> res0: Int = 1022
  countChange_new(300, coins)                     //> res1: Int = 1022

  //val coins = List(1, 2)
  //countChange(4, coins)
}