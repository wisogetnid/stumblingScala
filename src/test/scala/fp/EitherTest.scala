package fp

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

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

  "sequence" should "transform a list of eithers in either a list on all rights" in {
    val rights: List[MyEither[Nothing, Int]] = List(MyRight(1), MyRight(2), MyRight(3))
    MyEither.sequence(rights) shouldBe MyRight(List(1, 2, 3))
  }

  it should "return a left if there is a left in the list" in {
    val leftAndRights: List[MyEither[String, Int]] = List(MyRight(1), MyLeft("zwoa"), MyRight(3))
    MyEither.sequence(leftAndRights) shouldBe MyLeft("zwoa")
  }

  "traverse" should "apply a function on a list of eithers and return an either of a list if all is successful" in {
    val rights = List(2, 4, 6)
    MyEither.traverse(rights)(divider) shouldBe MyRight(List(1, 2, 3))
  }

  it should "return a left if the application of the function leads to a left" in {
    val leftAndRights = List(2, 4, 5)
    MyEither.traverse(leftAndRights)(divider) shouldBe MyLeft("boum")
  }

  private def divider(a: Int): MyEither[String, Int] = if (a % 2 == 0) MyRight(a / 2) else MyLeft("boum")
}
