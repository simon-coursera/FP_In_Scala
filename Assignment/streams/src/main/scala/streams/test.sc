package streams

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  class SolutionChecker extends GameDef with Solver with StringParserTerrain {

		val level = """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
  }

	val s = new SolutionChecker               //> s  : streams.test.SolutionChecker = streams.test$$anonfun$main$1$SolutionChe
                                                  //| cker$1@2cd652c1
   }