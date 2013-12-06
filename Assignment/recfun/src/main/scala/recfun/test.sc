package recfun

object test {

  def pascal(c: Int, r: Int): Int =
    if (c < 0 || c > r) 0 else if (c == 0 && r == 0) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
                                                  //> pascal: (c: Int, r: Int)Int

  pascal(0, 2)                                    //> res0: Int = 1
  pascal(1, 2)                                    //> res1: Int = 2
  pascal(1, 3)                                    //> res2: Int = 3


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
    if (parentheseChars.isEmpty) return true

    var iHead = 0
    while (iHead < parentheseChars.length && iHead >= 0) {
      iHead = matchParenthese(parentheseChars, iHead)
    }

    iHead != -1
  }                                               //> balance: (chars: List[Char])Boolean


  val name: List[Char] = "())(".toList            //> name  : List[Char] = List((, ), ), ()

  balance(name)                                   //> res3: Boolean = false
}