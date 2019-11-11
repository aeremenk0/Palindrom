object Palindrome {
//  def isPalindrome(s: List[Char]): Boolean = {
//    def go(a: Array[Char], front: Int, end: Int):Boolean = {
//      if(front >= end ) true
//      else
//        if (a(front) != a(end)) false
//        else
//          go(a, front + 1, end - 1)
//    }
//
//    if(s.isEmpty) false
//    else go(s.toArray, 0, s.length - 1)
//  }

//  def gen(head:List[Char], tail: List[Char]): List[String] = {
//    tail match {
//      case Nil if isPalindrome(head) => List((head).reverse.mkString)
//      case h::t if isPalindrome(head) =>
//        gen(List(h), t).map(s => (head).reverse.mkString + "|" + s.mkString) ++ gen(h :: head, t)
//      case h::t => gen(h :: head, t)
//      case Nil => Nil
//    }
//  }


  def apply(s: String): List[String] = {
    val m = getPalindromsMap(s.toCharArray)
    build(0, m)
  }

  def longestPalindrome(s: String): String = {
    val m = getPalindromsMap(s.toCharArray)
    val res = build(0, m).filterNot(_ == s)
    res.maxBy(_.size)
  }

  def build(i: Int, m: Map[Int, List[Pali]]) : List[String] = {
    val x = m.getOrElse(i, List())

    if(x.isEmpty) List("")
    else x.flatMap(s => build(s.end, m).map(a => s.text + a))
  }

  case class Pali(start: Int, end: Int, text: String)

  def checkForPalindroms(a: Array[Char], s1: Int, s2: Int, acc: List[Pali]): List[Pali] = {
    if(s1 < 0 || s2 >= a.size) {
      acc
    }
    else if (a(s1) == a(s2)){
      val p = Pali(s1, s2+1, a.slice(s1, s2+1).mkString)
      checkForPalindroms(a, s1-1, s2+1, p::acc)
    }
    else {
      acc
    }
  }

  def getPalindromsMap(s: Array[Char]): Map[Int, List[Pali]] = {
    val r = Range(0, s.size)
    val pl = r.foldRight(Nil:List[Pali]){(i, acc) =>
      checkForPalindroms(s, i, i, checkForPalindroms(s, i, i+1, acc))
    }
    pl.groupBy(x=>x.start)
  }
}