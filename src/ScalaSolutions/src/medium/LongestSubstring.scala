package medium

object LongestSubstring extends App {
  def lengthOfLongestSubstring(s: String): Int =
    s.foldRight(List.empty[Char])((x, acc) =>
      if (acc.contains(x)) acc else x :: acc
    ).length

  /** Find the longest continuous substring `l` without repeating characters.
    *
    * @param s
    *   String
    * @return
    *   The result lenght.
    */
  def longestSubstring(s: String): String = {
    def go(
        input: List[Char],
        stack: List[Char]
    ): String = (input, stack) match {
      case (Nil, Nil) => ""
      case (Nil, _)   => ""
      case (x :: xs, stack) if !stack.contains(x) =>
        x.toString + longestSubstring(go(xs, x :: stack).mkString)
      case (_, _) => ""
    }

    go(s.toList, Nil).mkString
  }

  /** Using an array to store `ASCII` character indexes. ChatGPT solution.
    */
  def lengthOfLongestSubstringG(s: String): Int = {
    val n = s.length
    var maxLength = 0
    var start = 0
    val charIndex =
      Array.fill(256)(-1)

    for (end <- 0 until n) {
      val char = s(end)
      if (charIndex(char) >= start) {
        start = charIndex(char) + 1
      }
      charIndex(char) = end
      maxLength = Math.max(maxLength, end - start + 1)
    }

    maxLength
  }

  println(lengthOfLongestSubstringG("abcaagbf"))
}
