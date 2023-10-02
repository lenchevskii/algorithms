package easy

object LongestCommonPrefix extends App {
  def longestCommonPrefix(strings: Array[String]): String = {
    if (strings.isEmpty) ""
    else
      strings.reduceLeft { (prefix, str) =>
        prefix.zip(str).takeWhile { case (a, b) => a == b }.map(_._1).mkString
      }
  }

  val wordsList = Array("apple", "appetizer", "apparatus")
  println(longestCommonPrefix(wordsList))
}
