package recfun

object test {
  def power(x: Double) = x * x                    //> power: (x: Double)Double

  def balance(chars: List[Char]): Boolean = {
    //val remainingChars: List[Char] = chars
    def matchParences(chars: List[Char]): List[Char] = {
      if (chars.head != '(') return List()

      val remainChars = chars.tail
      if (remainChars.head == ')') remainChars
      else {
        if (remainChars.head == '(') matchParences(remainChars.tail)
        else List()
      }
      List()
    }

    if (chars.isEmpty) return true

    if (chars.head == ')') false
    else {
      if (chars.head == '(') {
        val remainChars = matchParences(chars.tail)

        if (remainChars.isEmpty) false else balance(remainChars.tail)
      } else balance(chars.tail)
    }
  }                                               //> balance: (chars: List[Char])Boolean
}