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

  val intOrder: (Int, Int) => Boolean = _ >= _
  val stringOrder: (String, String) => Boolean = _ >= _

  "isSorted validator" should "validate an ordered array of integers" in {
    Recursion.isSorted[Int](Array(9, 7, 3, 2, 1), intOrder) shouldBe true
  }

  it should "notify an unordered array of integers" in {
    Recursion.isSorted[Int](Array(9, 7, 3, 15, 2, 1), intOrder) shouldBe false
  }

  it should "validate an ordered array of only one integer" in {
    Recursion.isSorted[Int](Array(9), intOrder) shouldBe true
  }

  it should "validate an ordered array of strings" in {
    Recursion.isSorted[String](Array("f", "e", "c", "a"), stringOrder) shouldBe true
  }

  it should "notify an unordered array of strings" in {
    Recursion.isSorted[String](Array("a", "f", "z"), stringOrder) shouldBe false
  }
}
