package recfun

object test1 {

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
  }                                               //> countChange: (money: Int, coins: List[Int])Int

  val coins = List(5,10,20,50,100,200,500)        //> coins  : List[Int] = List(5, 10, 20, 50, 100, 200, 500)
  countChange(300, coins)                         //> res0: Int = 1022

  //val coins = List(1, 2)
  //countChange(4, coins)
}