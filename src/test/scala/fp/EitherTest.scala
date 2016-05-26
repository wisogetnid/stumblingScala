package fp

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {

  "map" should "perform a function on the right value" in {
    val right = MyRight(3)
    right.map(_ + 4) shouldBe MyRight(7)
  }

  it should "return left unchanged" in {
    val left: MyEither[String, Int] = MyLeft("some")
    left.map(_ + "thing") shouldBe MyLeft("some")
  }

  "flatMap" should "perform a function on the right value" in {
    val right: MyEither[String, String] = MyRight("begin")
    right.flatMap(a => MyRight(a + "ning")) shouldBe MyRight("beginning")
  }

  it should "return left when a function returning left gets applied on a right value" in {
    val right: MyEither[String, String] = MyRight("right")
    right.flatMap(a => MyLeft("leftie")) shouldBe MyLeft("leftie")
  }

  it should "return left if executed on a left value" in {
    val left: MyEither[String, String] = MyLeft("left")
    left.flatMap(a => MyRight("good")) shouldBe MyLeft("left")
  }

  "orElse" should "provide a default case in case of lefts" in {
    val eitherWithElse: MyEither[String, String] = MyLeft("fail").orElse("nonFail")
    eitherWithElse shouldBe MyRight("nonFail")
  }

  it should "not influence a right case" in {
    val rightWithElse: MyEither[String, String] = MyRight("success").orElse("partialSuccess")
    rightWithElse shouldBe MyRight("success")
  }

  "map2" should "combine two rights" in {
    val aRight = MyRight("happy")
    val anotherRight = MyRight("hippo")
    aRight.map2(anotherRight)(_ + " " + _) shouldBe MyRight("happy hippo")
  }

  it should "return left if one of the arguments is left" in {
    val right: MyEither[String, String] = MyRight("happy")
    val left: MyEither[String, String] = MyLeft("sad")
    right.map2(left)(_ + _) shouldBe MyLeft("sad")
    left.map2(right)(_ + _) shouldBe MyLeft("sad")
  }
}
