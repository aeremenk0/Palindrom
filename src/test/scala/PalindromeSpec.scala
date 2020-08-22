import org.scalatest.{FlatSpec, Matchers}

class PalindromeSpec extends FlatSpec with Matchers {
  "Palindrome.longestPalindrome" should "return the empty sting on empty input" in {
    Palindrome.longestPalindrome("") should be("")
  }

  "Palindrome.longestPalindrome" should "return the one symbol sting on non-palindromic input" in {
    Palindrome.longestPalindrome("abcdef").size should be(1)
  }

  "Palindrome.longestPalindrome" should "return the longest palindromic string" in {
    Palindrome.longestPalindrome("babacd") should be("bab")
  }
}

