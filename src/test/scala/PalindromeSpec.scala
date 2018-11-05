import org.scalatest.{FlatSpec, Matchers}

class PalindromeSpec extends FlatSpec with Matchers {
  "Palindrome.isPalindrome" should "return true if a string is a palindrome" in {
    assert(Palindrome.isPalindrome("a".toList))
    assert(Palindrome.isPalindrome("aa".toList))
    assert(Palindrome.isPalindrome("aba".toList))
    assert(Palindrome.isPalindrome("abcba".toList))
  }

  "Palindrome.isPalindrome" should "return false if a string is empty" in {
    Palindrome.isPalindrome("".toList) shouldBe false
  }

  "Palindrome.isPalindrome" should "return false if a string is not a palindrome " in {
    Palindrome.isPalindrome("ab".toList) shouldBe false
    Palindrome.isPalindrome("abb".toList) shouldBe false
    Palindrome.isPalindrome("aba".toList) shouldBe true
  }

  "Palindrome.apply" should "return list of all palindroms" in {
    Palindrome("ab").size should be(1)
    Palindrome("aa").size should be(2)
    Palindrome("abba").size should be(3)
    Palindrome("aabaa").size should be(6)
    Palindrome("aaaa").size should be(8)
  }

  "Palindrome.apply" should "return the same result as oldApply" in {
    Palindrome("ab").size should be(Palindrome.oldApply("ab").size)
    Palindrome("aa").size should be(Palindrome.oldApply("aa").size)
    Palindrome("abba").size should be(Palindrome.oldApply("abba").size)
    Palindrome("aabaa").size should be(Palindrome.oldApply("aabaa").size)
    Palindrome("aaaa").size should be(Palindrome.oldApply("aaaa").size)
  }

  "Palindrome.checkForPalindroms" should "return list of palindroms" in {
    Palindrome.checkForPalindroms("ab".toCharArray, 0, 0, Nil).size should be(1)
    Palindrome.checkForPalindroms("ab".toCharArray, 0, 1, Nil).size should be(0)
    Palindrome.checkForPalindroms("aa".toCharArray, 0, 1, Nil).size should be(1)
    Palindrome.checkForPalindroms("ab".toCharArray, 1, 1, Nil).size should be(1)
    Palindrome.checkForPalindroms("aaaaaaa".toCharArray, 3, 4, Nil).size should be(3)
    Palindrome.checkForPalindroms("aaaaaaa".toCharArray, 3, 3, Nil).size should be(4)
  }
}
