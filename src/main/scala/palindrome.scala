object Palindrome {
  def longestPalindrome(s: String): String = {
    if(s.isEmpty) ""
    else {
      val a = s.toArray
      val r = Range(0, s.size)
      val pl = r.foldRight(Nil:List[(Int, Int)]){(i, acc) =>
        expandAroundCenter(a, i, i, (i, i))::expandAroundCenter(a, i, i+1, (i, i))::acc
      }
      val z = pl.maxBy(x => Math.abs(x._2 - x._1))
      a.slice(z._1, z._2+1).mkString
    }
  }

  // find the longest palindromic string starting from s1 and s2
  def expandAroundCenter(a: Array[Char], s1: Int, s2: Int, acc: (Int, Int)): (Int, Int) = {
    if (s1 < 0 || s2 >= a.size || a(s1) != a(s2)) {
      acc
    } else {
      expandAroundCenter(a, s1 - 1, s2 + 1, (s1, s2))
    }
  }
}