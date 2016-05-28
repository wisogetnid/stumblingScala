package fp

import org.scalatest.{FlatSpec, Matchers}

class MyStreamTest extends FlatSpec with Matchers{

  "toList" should "convert a stream into a List" in {
    val stream = MyStream(1, 2, 3)
    stream.toList shouldBe List(1, 2, 3)
  }

  "take(n)" should "return the first n elements" in {
    val stream = MyStream(1, 2, 3)
    stream.take(2).toList shouldBe List(1, 2)
  }

  "drop(n)" should "drop the first n elements" in {
    val stream = MyStream(1, 2, 3)
    stream.drop(2).toList shouldBe List(3)
  }

  "takeWhile" should "return leading elements of a stream while the predicate matches" in {
    val stream = MyStream(1, 3, 4, 5)
    stream.takeWhile(_ % 2 == 1).toList shouldBe List(1, 3)
  }
}
