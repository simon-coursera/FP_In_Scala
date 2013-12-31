package forcomp
import Anagrams._

object test {
  List("a", "b", "c")                             //> res0: List[String] = List(a, b, c)
  val os = wordOccurrences("aabb")                //> os  : forcomp.Anagrams.Occurrences = List((a,2), (b,2))
  os.tail                                         //> res1: List[(Char, Int)] = List((b,2))
  //dictionaryByOccurrences
  for { o <- os; c <- 1 to o._2 } yield (o._1 -> c)
                                                  //> res2: List[(Char, Int)] = List((a,1), (a,2), (b,1), (b,2))
  val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 13))
                                                  //> x  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,13))
  val y = List(('a', 1), ('d', 1), ('l', 1), ('r', 13))
                                                  //> y  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,13))

  subtract(x, y)                                  //> res3: forcomp.Anagrams.Occurrences = List()
  
  val abba = List(('a', 1), ('b', 2))             //> abba  : List[(Char, Int)] = List((a,1), (b,2))
  combinations(abba)                              //> res4: List[forcomp.Anagrams.Occurrences] = List(List(), List((a,1)), List((b
                                                  //| ,1)), List((b,2)), List((a,1), (b,1)), List((a,1), (b,2)))
  dictionary.length                               //> res5: Int = 45374
  
  sentenceAnagrams(List("I", "love", "you"))      //> res6: List[forcomp.Anagrams.Sentence] = List(List(Io, Lev, you), List(Io, yo
                                                  //| u, Lev), List(Lev, Io, you), List(Lev, you, Io), List(olive, you), List(you,
                                                  //|  Io, Lev), List(you, Lev, Io), List(you, olive))
  sentenceAnagrams(List("lives"))                 //> res7: List[forcomp.Anagrams.Sentence] = List(List(is, Lev), List(Lev, is), L
                                                  //| ist(Elvis), List(evils), List(Levis), List(lives), List(veils))
  sentenceAnagrams(List("Yes", "man"))            //> res8: List[forcomp.Anagrams.Sentence] = List(List(en, as, my), List(en, my, 
                                                  //| as), List(man, yes), List(men, say), List(as, en, my), List(as, my, en), Lis
                                                  //| t(sane, my), List(Sean, my), List(my, en, as), List(my, as, en), List(my, sa
                                                  //| ne), List(my, Sean), List(say, men), List(yes, man))
}