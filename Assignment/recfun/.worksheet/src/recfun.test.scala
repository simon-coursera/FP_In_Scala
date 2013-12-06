package recfun

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(60); 
  def power(x: Double) = x * x;System.out.println("""power: (x: Double)Double""");$skip(682); 

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
  };System.out.println("""balance: (chars: List[Char])Boolean""")}
}
