package medium

object ReverseInteger extends App {
  def reverse(x: Int): Int = {
    val s = if (x < 0) x.toString.tail else x.toString
    val d = s.reverse.toDouble * x.signum
    if (d.isValidInt) d.toInt else 0
  }
}
