package easy

object ReverseWord extends App {
  def reverseWords(s: String): String =
    s.split("\\s+").toList.map(r => r.reverse).mkString(" ")

  println(reverseWords("Let's take LeetCode contest"))
}
