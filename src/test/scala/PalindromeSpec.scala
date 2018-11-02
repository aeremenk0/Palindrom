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
  }
}
