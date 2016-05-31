package fp

import org.scalatest.{FlatSpec, Matchers}

class InfiniteStreamTest extends FlatSpec with Matchers{
  "constant" should "create an infinite stream of a specific value" in {
    MyStream.constant("a").take(3).toList shouldBe List("a", "a", "a")
  }

  "uConstant" should "behave as constant but be implemented using unfold" in {
    MyStream.uConstant("a").take(3).toList shouldBe List("a", "a", "a")
  }

  "from" should "generate an infinite stream of integers starting from n" in {
    MyStream.from(3).take(3).toList shouldBe List(3, 4, 5)
  }

  "uFrom" should "behave as from but be implemented using unfold" in {
    MyStream.uFrom(3).take(3).toList shouldBe List(3, 4, 5)
  }

  "fibonacci" should "generate an infinite stream of fibonacci numbers" in {
    MyStream.fibonacci.drop(5).take(5).toList shouldBe List(5, 8, 13, 21, 34)
  }

  "uFibonacci" should "behave as fibonacci but be implemented using unfold" in {
    MyStream.uFibonacci.drop(5).take(5).toList shouldBe List(5, 8, 13, 21, 34)
  }

  "unfold" should "provide a general way of building streams" in {
    MyStream.unfold("a")(a => Some(a, a + "a")).take(3).toList shouldBe List("a", "aa", "aaa")
  }
}
