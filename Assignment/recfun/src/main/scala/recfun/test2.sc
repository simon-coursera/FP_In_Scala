package recfun

object test2 {

  var l = List(1, 2, 6, 9, 3, 7)                  //> l  : List[Int] = List(1, 2, 6, 9, 3, 7)

  l.sortWith(_>=_)                                //> res0: List[Int] = List(9, 7, 6, 3, 2, 1)
  
  12 % 3                                          //> res1: Int(0) = 0
}