package sandbox

import org.scalatest.{Matchers, FlatSpec}

class SandboxTest extends FlatSpec with Matchers {

  "something" should "do something" in {
    helper(List("stome", "zeuts", "works?", "und", "noch was")) shouldBe (true)
  }


  def helper(input: Any) = {
    input match {
      case List("something", "soome", _*) => true
      case List("stome", firstValue @ _, "works?", otherValues @ _*) => {print(s"first ${firstValue} others ${otherValues}"); true}
      case _ => false
    }
  }

}
