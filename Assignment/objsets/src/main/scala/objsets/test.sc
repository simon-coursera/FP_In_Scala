package objsets

import common._
import TweetReader._

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }                                               //> asSet: (tweets: objsets.TweetSet)Set[objsets.Tweet]

  def size(set: TweetSet): Int = asSet(set).size  //> size: (set: objsets.TweetSet)Int

  val set1 = new Empty                            //> set1  : objsets.Empty = objsets.Empty@58cdd92e
  val set2 = set1.incl(new Tweet("a", "a body", 20))
                                                  //> set2  : objsets.TweetSet = objsets.NonEmpty@5fbc4e89
  val set3 = set2.incl(new Tweet("b", "b body", 20))
                                                  //> set3  : objsets.TweetSet = objsets.NonEmpty@578cad92
  val c = new Tweet("c", "c body", 7)             //> c  : objsets.Tweet = User: c
                                                  //| Text: c body [7]
  val d = new Tweet("d", "d body", 9)             //> d  : objsets.Tweet = User: d
                                                  //| Text: d body [9]
  val set4c = set3.incl(c)                        //> set4c  : objsets.TweetSet = objsets.NonEmpty@54d77925
  val set4d = set3.incl(d)                        //> set4d  : objsets.TweetSet = objsets.NonEmpty@e5297a7
  val set5 = set4c.incl(d)                        //> set5  : objsets.TweetSet = objsets.NonEmpty@50625c49

  val m = set5.mostRetweeted                      //> m  : objsets.Tweet = User: a
                                                  //| Text: a body [20]
  
  GoogleVsApple.trending.foreach( x=> println(x.retweets))
                                                  //> 321
                                                  //| 290
                                                  //| 205
                                                  //| 202
                                                  //| 198
                                                  //| 191
                                                  //| 180
                                                  //| 139
                                                  //| 131
                                                  //| 131
                                                  //| 125
                                                  //| 121
                                                  //| 120
                                                  //| 114
                                                  //| 111
                                                  //| 108
                                                  //| 101
                                                  //| 95
                                                  //| 94
                                                  //| 93
                                                  //| 93
                                                  //| 85
                                                  //| 79
                                                  //| 79
                                                  //| 79
                                                  //| 79
                                                  //| 78
                                                  //| 76
                                                  //| 76
                                                  //| 76
                                                  //| 68
                                                  //| 67
                                                  //| 67
                                                  //| 66
                                                  //| 66
                                                  //| 65
                                                  //| 63
                                                  //| 63
                                                  //| 61
                                                  //| 57
                                                  //| 56
                                                  //| 55
                                                  //| 54
                                                  //| 52
                                                  //| 50
                                                  //| 49
                                                  //| 49
                                                  //| 49
                                                  //| 49
                                                  //| 48
                                                  //| 48
                                                  //| 48
                                                  //| 47
                                                  //| 47
                                                  //| 47
                                                  //| 47
                                                  //| 46
                                                  //| 45
                                                  //| 45
                                                  //| 43
                                                  //| 43
                                                  //| 42
                                                  //| 42
                                                  //| 42
                                                  //| 42
                                                  //| 41
                                                  //| 41
                                                  //| 39
                                                  //| 37
                                                  //| 37
                                                  //| 37
                                                  //| 36
                                                  //| 35
                                                  //| 35
                                                  //| 35
                                                  //| 34
                                                  //| 33
                                                  //| 33
                                                  //| 33
                                                  //| 33
                                                  //| 33
                                                  //| 33
                                                  //| 32
                                                  //| 32
                                                  //| 32
                                                  //| 32
                                                  //| 32
                                                  //| 31
                                                  //| 31
                                                  //| 31
                                                  //| 31
                                                  //| 30
                                                  //| 30
                                                  //| 30
                                                  //| 29
                                                  //| 29
                                                  //| 27
                                                  //| 27
                                                  //| 27
                                                  //| 26
                                                  //| 26
                                                  //| 25
                                                  //| 25
                                                  //| 25
                                                  //| 25
                                                  //| 25
                                                  //| 25
                                                  //| 25
                                                  //| 25
                                                  //| 25
                                                  //| 24
                                                  //| 24
                                                  //| 23
                                                  //| 23
                                                  //| 22
                                                  //| 22
                                                  //| 22
                                                  //| 22
                                                  //| 21
                                                  //| 21
                                                  //| 21
                                                  //| 21
                                                  //| 21
                                                  //| 20
                                                  //| 19
                                                  //| 19
                                                  //| 19
                                                  //| 19
                                                  //| 18
                                                  //| 18
                                                  //| 18
                                                  //| 18
                                                  //| 18
                                                  //| 17
                                                  //| 17
                                                  //| 17
                                                  //| 16
                                                  //| 16
                                                  //| 16
                                                  //| 16
}