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
    val right = MyRight("begin")
    right.flatMap(a => MyRight(a + "ning")) shouldBe MyRight("beginning")
  }

  it should "return left when a function returning left gets applied on a right value" in {
    val right = MyRight("right")
    right.flatMap(a => MyLeft("leftie")) shouldBe MyLeft("leftie")
  }
  
  it should "return left if executed on a left value" in {
    val left = MyLeft("left")
    left.flatMap(a => MyRight("good")) shouldBe MyLeft("left")
  }
}
