package fp

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class MyOptionTest extends FlatSpec with Matchers {
  val some: MyOption[String] = MySome("test")

  "map" should "allow to execute a function on the value" in {
    some.map(_ + "2") shouldBe MySome("test2")
  }

  "flatMap" should "flatten two MySome into one while executing a function" in {
    some.flatMap(a => MySome(a +"3")) shouldBe MySome("test3")
  }

  it should "return MyNone if the applied function returned a MyNone" in {
    some.flatMap(_ => MyNone) shouldBe MyNone
  }

  "getOrElse" should "return the content or a default value immediately" in {
    some.getOrElse("default") shouldBe "test"
    MyNone.getOrElse("default") shouldBe "default"
  }

  "orElse" should "set a default value for None without immediate execution" in {
    some.orElse("default")
    some.getOrElse("fail") shouldBe "test"
  }

  it should "return the set default value as soon as it's called" in {
    val noneWithElse = MyNone.orElse("default")
    noneWithElse.getOrElse("fail") shouldBe "default"
  }

  "filter" should "only keep values that match the predicate" in {
    some.filter(_.startsWith("t")) shouldBe MySome("test")
    some.filter(_.startsWith("r")) shouldBe MyNone
  }

  "map2" should "merge two optional values using a function" in {
    MyOption.map2(MySome(3), MySome(2))(_ + _) shouldBe MySome(5)
  }

  it should "return None if at least one of the operands is None" in {
    MyOption.map2(MySome(5), MyNone)(_ + _) shouldBe MyNone
  }

  "sequence" should "flatten a list of some" in {
    MyOption.sequence(List(MySome(2), MySome(3), MySome(1))) shouldBe MySome(List(2,3,1))
  }

  it should "return None if at least one none value is in the list" in {
    MyOption.sequence(List(MySome(4), MyNone, MySome(3))) shouldBe MyNone
  }

  "traverse" should "apply a function (returning Option) on every element of a list" in {
    MyOption.traverse(List(1, 2, 5))(a => Try(10 / a).toOption) shouldBe Some(List(10, 5, 2))
  }

  it should "return None when the applied function returns None for at least one value" in {
    MyOption.traverse(List(0, 1, 2))(a => Try(10 / a).toOption) shouldBe None
  }
}
