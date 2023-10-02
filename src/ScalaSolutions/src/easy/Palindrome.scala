package easy

object Palindrome extends App {
  def isPalindrome(i: Int): Boolean =
    i.toString == i.toString.reverse
}
