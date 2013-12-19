package patmat
import patmat.Huffman._

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val sampleTree = makeCodeTree(
    makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
    Leaf('t', 2))                                 //> sampleTree  : patmat.Huffman.Fork = Fork(Fork(Leaf(x,1),Leaf(e,1),List(x, e)
                                                  //| ,2),Leaf(t,2),List(x, e, t),4)


	val trees = makeOrderedLeafList(List(('a',4), ('b',2), ('c', 5), ('c', 1)))
                                                  //> trees  : List[patmat.Huffman.Leaf] = List(Leaf(c,1), Leaf(b,2), Leaf(a,4), L
                                                  //| eaf(c,5))
	until(singleton, combine)(trees)          //> res0: List[patmat.Huffman.CodeTree] = List(Fork(Leaf(c,5),Fork(Fork(Leaf(c,1
                                                  //| ),Leaf(b,2),List(c, b),3),Leaf(a,4),List(c, b, a),7),List(c, c, b, a),12))
  var tree = createCodeTree("aabbccc".toList)     //> tree  : patmat.Huffman.CodeTree = Fork(Leaf(c,3),Fork(Leaf(a,2),Leaf(b,2),Li
                                                  //| st(a, b),4),List(c, a, b),7)
	decode(tree, List(0, 1,0,0,1,1))          //> res1: List[Char] = List(c, a, c, b)
	decode(frenchCode, secret)                //> res2: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
	
	//decode(tree, )
val a = List(('a', List(1)))                      //> a  : List[(Char, List[Int])] = List((a,List(1)))
val b = List(('b', List(0)))                      //> b  : List[(Char, List[Int])] = List((b,List(0)))
mergeCodeTables(a,b)                              //> res3: patmat.Huffman.CodeTable = List((a,List(0, 1)), (b,List(1, 0)))

convert(tree)                                     //> res4: patmat.Huffman.CodeTable = List((c,List(0)), (b,List(1, 1)), (a,List(1
                                                  //| , 0)))

decode(tree, quickEncode(tree)("aabbcccc".toList))//> res5: List[Char] = List(a, a, b, b, c, c, c, c)
}