package easy

case class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object ListNode {
  def unapply(node: ListNode): Option[(Int, Option[ListNode])] =
    Some((node._x, Some(node.next)))
}

object MergeLists extends App {
  def mergeLists[A](l1: List[A], l2: List[A])(implicit
      ord: Ordering[A]
  ): List[A] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (xs, Nil)  => xs
    case (Nil, ys)  => ys
    case (x :: xs, y :: ys) =>
      if (ord.lt(x, y))
        x :: mergeLists(xs, l2)
      else
        y :: mergeLists(l1, ys)
  }

  // def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {
  //   (list1, list2) match {
  //     case (null, null) => null
  //     case (null, l2)   => l2
  //     case (l1, null)   => l1
  //     case (ListNode(x1, next1), ListNode(x2, next2)) =>
  //       if (x1 < x2) {
  //         val mergedTail = mergeTwoLists(next1.get, list2)
  //         new ListNode(x1) {
  //           next = mergedTail
  //         }
  //       } else {
  //         val mergedTail = mergeTwoLists(list1, next2.get)
  //         new ListNode(x2) {
  //           next = mergedTail
  //         }
  //       }
  //   }
  // }

  // val list1 = new ListNode(1)
  // list1.next = new ListNode(3)
  // list1.next.next = new ListNode(5)

  // val list2 = new ListNode(2)
  // list2.next = new ListNode(4)
  // list2.next.next = new ListNode(6)

  // val mergedList = mergeTwoLists(list1, list2)

  // var current = mergedList
  // while (current != null) {
  //   print(current._x + " -> ")
  //   current = current.next
  // }
}
