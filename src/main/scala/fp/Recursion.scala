package fp

object Recursion {
  def isSorted[A](array: Array[A], intOrder: (A, A) => Boolean) = ???

  def fib(n: Int): Int = {
    if (n <= 1)
      n
    else
      fib(n-1) + fib(n-2)
  }

}
