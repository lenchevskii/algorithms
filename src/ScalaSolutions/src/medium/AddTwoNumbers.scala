package medium

case class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object ListNode {
  def unapply(node: ListNode): Option[(Int, Option[ListNode])] =
    Some((node._x, Some(node.next)))
}

object AddTwoNumbers extends App {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    (l1, l2) match {
      case (null, null) => null
      case (null, _)    => l2
      case (_, null)    => l1
      case _ => {
        var carry = 0
        var dummy = new ListNode()
        var current = dummy

        var p1 = l1
        var p2 = l2

        while (p1 != null || p2 != null) {
          val x1 = if (p1 != null) p1.x else 0
          val x2 = if (p2 != null) p2.x else 0
          val sum = x1 + x2 + carry
          carry = sum / 10
          current.next = new ListNode(sum % 10)
          current = current.next

          if (p1 != null) p1 = p1.next
          if (p2 != null) p2 = p2.next
        }

        if (carry > 0) {
          current.next = new ListNode(carry)
        }

        dummy.next
      }
    }
  }

  val list1 = new ListNode(2)
  list1.next = new ListNode(4)
  list1.next.next = new ListNode(3)

  val list2 = new ListNode(5)
  list2.next = new ListNode(6)
  list2.next.next = new ListNode(4)

  val result = addTwoNumbers(list1, list2)

  var current = result
  while (current != null) {
    println(current.x)
    current = current.next
  }

}
