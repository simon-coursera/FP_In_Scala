package streams

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  class SolutionChecker extends Solver with StringParserTerrain {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    def getNeibors = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
    def testNewNeiborOnly = newNeighborsOnly(
      Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,

      Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))
  }

  val game = new SolutionChecker                  //> game  : streams.test.SolutionChecker = streams.test$$anonfun$main$1$Solution
                                                  //| Checker$1@71ae13c0
  game.getNeibors.toList                          //> res0: List[(streams.test.game.Block, List[streams.test.game.Move])] = List((
                                                  //| Block(Pos(1,2),Pos(1,3)),List(Right, Left, Up)), (Block(Pos(2,1),Pos(3,1)),L
                                                  //| ist(Down, Left, Up)))
	game.testNewNeiborOnly.toList             //> res1: List[(streams.test.game.Block, List[streams.test.game.Move])] = List((
                                                  //| Block(Pos(2,1),Pos(3,1)),List(Down, Left, Up)))
  //Stream(Stream(1), Stream(2)).flatten

	val s = Set(1,2)                          //> s  : scala.collection.immutable.Set[Int] = Set(1, 2)
	s + 3                                     //> res2: scala.collection.immutable.Set[Int] = Set(1, 2, 3)
}