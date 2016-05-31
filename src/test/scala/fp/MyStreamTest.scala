package fp

import fp.MyStream.cons
import org.scalatest.{FlatSpec, Matchers}

class MyStreamTest extends FlatSpec with Matchers {
  private val stream: MyStream[Int] = MyStream(1, 2, 3)

  "toList" should "convert a stream into a List" in {
    stream.toList shouldBe List(1, 2, 3)
  }

  "take(n)" should "return the first n elements" in {
    stream.take(2).toList shouldBe List(1, 2)
  }

  "drop(n)" should "drop the first n elements" in {
    stream.drop(2).toList shouldBe List(3)
  }

  "takeWhile" should "return leading elements of a stream while the predicate matches" in {
    val stream = MyStream(1, 3, 4, 5)
    stream.takeWhile(_ % 2 == 1).toList shouldBe List(1, 3)
  }

  "forall" should "validate a predicate on every element of a stream" in {
    val stream = MyStream(1, 3, 5)
    stream.forAll(_ % 2 == 1) shouldBe true
  }

  it should "return false (and terminate) on the first non matching element" in {
    val stream = cons(1, cons(2, cons({ fail("should not be executed"); 3}, cons(5, MyStream.empty))))
    //    val stream = MyStream(1, 2, {fail(); 3}, 5) doesn't work as all the elements get evaluated on instantiation
    stream.forAll(_ % 2 == 1) shouldBe false
  }

  "foldRight" should "do what it's supposed to" in {
    stream.foldRight(10)(_ + _) shouldBe 16
  }

  "headOption" should "return the first element of a stream" in {
    stream.headOption shouldBe Some(1)
  }

  it should "return None is no elements are in the stream" in {
    val stream: MyStream[Any] = MyEmpty
    stream.headOption shouldBe None
  }

  "map" should "execute a function on every element" in {
    stream.map(_ + 1).toList shouldBe List(2, 3, 4)
  }

  "filter" should "remove all elements not matching a predicate" in {
    stream.filter(_ % 2 == 0).toList shouldBe List(2)
  }

  "append" should "append a stream to another" in {
    val anotherStream = MyStream(4, 5)
    stream.append(anotherStream).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "flatMap" should "apply a function returning streams on each element of a stream and flatten the result" in {
    val wordStream = MyStream("bob", "eve")
    wordStream.flatMap(a => MyStream(a.split(""): _*)).toList shouldBe List("b", "o", "b", "e", "v", "e")
  }

  "uMap" should "behave like map but be implemented using unfold" in {
    stream.uMap(_ + 1).toList shouldBe List(2, 3, 4)
  }

  "uTake" should "behave like take but be implemented using unfold" in {
    stream.uTake(2).toList shouldBe List(1, 2)
  }

  "uTakeWhile" should "behave like takeWhile but be implemented using unfold" in {
    val stream = MyStream(1, 3, 4, 5)
    stream.uTakeWhile(_ % 2 == 1).toList shouldBe List(1, 3)
  }

  "zipWith" should "merge two streams using a function" in {
    stream.zipWith(stream)(_ + _).toList shouldBe List(2, 4, 6)
  }

  "zipAll" should "merge two streams creating tuples" in {
    stream.zipAll(stream).toList shouldBe List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)))
  }

  it should "add spare elements with a None as second tuple element" in {
    stream.zipAll(MyStream(1)).toList shouldBe List((Some(1), Some(1)), (Some(2), None), (Some(3), None))
    MyStream(1).zipAll(stream).toList shouldBe List((Some(1), Some(1)), (None, Some(2)), (None, Some(3)))
  }

}
