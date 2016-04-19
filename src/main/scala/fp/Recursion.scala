package fp

object Recursion {
  def isSorted[A](array: Array[A], isInOrder: (A, A) => Boolean): Boolean =
    if (array.length <= 1) true
    else if (isInOrder(array.head, array.tail.head))
      isSorted(array.tail.tail, isInOrder)
    else
      false

  def fib(n: Int): Int =
    if (n <= 1)
      n
    else
      fib(n-1) + fib(n-2)
}
