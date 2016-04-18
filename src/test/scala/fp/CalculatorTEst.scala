package fp

import org.scalatest.{FlatSpec, Matchers}

class CalculatorTest extends FlatSpec with Matchers {
  "Fionacci calculator" should "return correct 2nd number" in {
      Calculator.fib(2) shouldBe 1
   }

  it should "return correct 4th number" in {
    Calculator.fib(4) shouldBe 3
  }

  it should "return correct 10th number" in {
    Calculator.fib(10) shouldBe 21
  }
}
