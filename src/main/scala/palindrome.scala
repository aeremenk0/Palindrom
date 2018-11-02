object Palindrome {
  def isPalindrome(s: List[Char]): Boolean = {
    def go(a: Array[Char], front: Int, end: Int):Boolean = {
      if(front >= end ) true
      else
        if (a(front) != a(end)) false
        else
          go(a, front + 1, end - 1)
    }

    if(s.isEmpty)
      false
    else
      go(s.toArray, 0, s.length - 1)
  }

  def gen(head:List[Char], tail: List[Char]): List[String] = {
    tail match {
      case Nil if isPalindrome(head) => List((head).reverse.mkString)
      case h::t if isPalindrome(head) =>
        gen(List(h), t).map(s => (head).reverse.mkString + "|" + s.mkString) ++ gen(h :: head, t)
      case h::t => gen(h :: head, t)
      case Nil => Nil
    }
  }

  def apply(s: String): List[String] = {
    val cs = s.toList
    gen(List(cs.head), cs.tail)
  }
}