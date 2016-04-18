package fp

import org.scalatest.{FlatSpec, Matchers}

class RecursionTest extends FlatSpec with Matchers {
  "Fionacci calculator" should "return correct 2nd number" in {
      Recursion.fib(2) shouldBe 1
   }

  it should "return correct 4th number" in {
    Recursion.fib(4) shouldBe 3
  }

  it should "return correct 10th number" in {
    Recursion.fib(8) shouldBe 21
  }

  "isSorted validator" should "validate an ordered array of integers" in {
    val intOrder: (Int, Int) => Boolean = _ >= _

    Recursion.isSorted[Int](Array(9, 7, 3, 2, 1), intOrder)
  }
}
