package fp

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {

  "map" should "perform a function of the right value" in {
    val right = MyRight(3)
    right.map(_ + 4) shouldBe MyRight(7)
  }

  it should "return left unchanged" in {
    val left: MyEither[String, Int] = MyLeft("some")
    left.map(_ + "thing") shouldBe MyLeft("some")
  }
}
