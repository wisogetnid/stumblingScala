package fp

import org.scalatest.{FlatSpec, Matchers}

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
}
