package easy

import scala.collection.mutable.Stack

object ValidParentheses extends App {

  def isValid(s: String): Boolean = {
    val stack = Stack[Char]()
    val brackets = Map(')' -> '(', '}' -> '{', ']' -> '[')
    var isInputCorrect = true

    for (char <- s) {
      if (brackets.contains(char)) {
        val top = if (stack.isEmpty) '#' else stack.pop()
        if (top != brackets(char)) {
          isInputCorrect = false
        }
      } else {
        stack.push(char)
      }
    }

    isInputCorrect && stack.isEmpty
  }

  val input = "]"
  val result = isValid(input)
  println(s"The input string $input is valid: $result")
}
