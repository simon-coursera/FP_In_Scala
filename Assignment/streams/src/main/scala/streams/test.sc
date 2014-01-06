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
    def testPathsToGoal = pathsToGoal
    def testFrom = from(pathsFromStart, Set())
  }

  val game = new SolutionChecker                  //> game  : streams.test.SolutionChecker = streams.test$$anonfun$main$1$Solution
                                                  //| Checker$1@7b3d2b1c
  game.getNeibors.toList                          //> res0: List[(streams.test.game.Block, List[streams.test.game.Move])] = List((
                                                  //| Block(Pos(1,2),Pos(1,3)),List(Right, Left, Up)), (Block(Pos(2,1),Pos(3,1)),L
                                                  //| ist(Down, Left, Up)))
	game.testNewNeiborOnly.toList             //> res1: List[(streams.test.game.Block, List[streams.test.game.Move])] = List((
                                                  //| Block(Pos(2,1),Pos(3,1)),List(Down, Left, Up)))

  game.testPathsToGoal.toList                     //> res2: List[(streams.test.game.Block, List[streams.test.game.Move])] = List((
                                                  //| Block(Pos(4,7),Pos(4,7)),List(Down, Right, Right, Right, Down, Right, Right)
                                                  //| ), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Down, Right, Right, Down, Rig
                                                  //| ht)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Right, Right, Down, Down, 
                                                  //| Right)), (Block(Pos(4,7),Pos(4,7)),List(Down, Right, Right, Right, Down, Rig
                                                  //| ht, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Down, Right, Right,
                                                  //|  Down, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Right, Right, Do
                                                  //| wn, Down, Right)))
  //game.testFrom.take(20).toList
}