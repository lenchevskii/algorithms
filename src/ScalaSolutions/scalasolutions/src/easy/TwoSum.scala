package easy

def twoSum(nums: Array[Int], y: Int): Array[Int] = {
  def twoSumR(xs: List[(Int, Int)]): Array[Int] = xs match {
    case Nil => Array.empty[Int]
    case x1 :: xsTail =>
      val answer = twoSumH(y - x1._1, xsTail)
      if (answer.isEmpty) twoSumR(xsTail)
      else Array(x1._2, answer.head._2)
  }

  def twoSumH(target: Int, xs: List[(Int, Int)]): List[(Int, Int)] =
    xs match {
      case Nil => Nil
      case x2 :: xsTail =>
        if (x2._1 == target) x2 :: Nil
        else twoSumH(target, xsTail)
    }

  twoSumR(nums.zipWithIndex.toList)
}

object TwoSum extends App {
  println(twoSum(Array(1, 2, 3, 4, 10, 11, 12, 2, 3), 5).toList)
}
