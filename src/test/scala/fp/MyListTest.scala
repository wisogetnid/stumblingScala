package fp

import org.scalatest.{FlatSpec, Matchers}

class MyListTest extends FlatSpec with Matchers {

  "MyList" should "return the first element on head" in {
    Cons(1, MyNil).head shouldBe 1
  }

  it should "return MyNil on tail with only one element" in {
    Cons(1, MyNil).tail shouldBe MyNil
  }

  it should "take variadic elements in constructor" in {
    MyList(1,2,3) shouldBe Cons(1, Cons(2, Cons(3, MyNil)))
  }

  it should "be able to add elements" in {
    val list: MyList[Int] = MyList(1)
    list.add(2) shouldBe MyList(2,1)
  }

  it should "be able to remove first element" in {
    val list: MyList[Int] = MyList(1,2,3)
    list.remove(1) shouldBe MyList(2,3)
  }

  it should "be able to remove last element" in {
    val list: MyList[Int] = MyList(1,2,3)
    list.remove(3) shouldBe MyList(1,2)
  }

  it should "be able to drop n elements from the beginning" in {
    val list: MyList[Int] = MyList(1,2,3)
    list.drop(2) shouldBe MyList(3)
  }

  it should "be able to drop the last element with init" in {
    val list: MyList[Int] = MyList(1,2,3)
    list.init shouldBe MyList(1,2)
  }
}
