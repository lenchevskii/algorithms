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

  // println(lengthOfLongestSubstring("abcaagbf"))
}
