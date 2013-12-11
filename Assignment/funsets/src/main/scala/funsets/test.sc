package funsets

object test {
  //import FunSets._

  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: Int => Boolean, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = x => x == elem
                                                  //> singletonSet: (elem: Int)Int => Boolean

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = x => s(x) || t(x)
                                                  //> union: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = x => s(x) && t(x)
                                                  //> intersect: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = x => s(x) && !t(x)
                                                  //> diff: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  def filter(s: Set, p: Int => Boolean): Set = x => s(x) && p(x)
                                                  //> filter: (s: Int => Boolean, p: Int => Boolean)Int => Boolean

  val bound = 1000                                //> bound  : Int = 1000
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }                                               //> forall: (s: Int => Boolean, p: Int => Boolean)Boolean

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
                                                  //> exists: (s: Int => Boolean, p: Int => Boolean)Boolean

  def map(s: Set, f: Int => Int): Set = x => exists(s, y => x == f(y))
                                                  //> map: (s: Int => Boolean, f: Int => Int)Int => Boolean
  
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: Int => Boolean)String

	val s1 = union(singletonSet(1), singletonSet(2))
                                                  //> s1  : Int => Boolean = <function1>
	toString(map(s1, x => x + 100))           //> res0: String = {101,102}



  //1,3,4,5,7,1000
  //1,2,3,4
  val d1 = union(union(union(union(union(singletonSet(1), singletonSet(3)), singletonSet(4)), singletonSet(5)), singletonSet(7)), singletonSet(1000))
                                                  //> d1  : Int => Boolean = <function1>
  val d2 = union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
                                                  //> d2  : Int => Boolean = <function1>
  val d3 = union(singletonSet(-1000), singletonSet(0))
                                                  //> d3  : Int => Boolean = <function1>
  toString(diff(d1, d2))                          //> res1: String = {5,7,1000}
  toString(diff(d2, d3))                          //> res2: String = {1,2,3,4}
  
  
  toString(map(d1, x => x - 2))                   //> res3: String = {-1,1,2,3,5,998}

 }