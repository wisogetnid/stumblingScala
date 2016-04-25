package fp

import org.scalatest.{FlatSpec, Matchers}

class MyListTest extends FlatSpec with Matchers {

  val list = MyList(1,2,3)

  "MyList" should "return the first element on head" in {
    Cons(1, MyNil).head shouldBe 1
  }

  it should "return MyNil on tail with only one element" in {
    Cons(1, MyNil).tail shouldBe MyNil
  }

  it should "take variadic elements in constructor" in {
    list shouldBe Cons(1, Cons(2, Cons(3, MyNil)))
  }

  it should "add elements" in {
    list.add(2) shouldBe MyList(2,1,2,3)
  }

  it should "remove first element" in {
    list.remove(1) shouldBe MyList(2,3)
  }

  it should "remove last element" in {
    list.remove(3) shouldBe MyList(1,2)
  }

  it should "drop n elements from the beginning" in {
    list.drop(2) shouldBe MyList(3)
  }

  it should "drop the last element with init" in {
    list.init shouldBe MyList(1,2)
  }

  "foldRight" should "sum up" in {
    MyList.foldRight(list, 0)(_ + _) shouldBe 6
  }

  it should "make a product" in {
    MyList.foldRight(list, 1)(_ * _) shouldBe 6
  }

  it should "mirror itself" in {
    MyList.foldRight(list, MyNil:MyList[Int])(Cons(_,_)) shouldBe MyList(1,2,3)
  }

  it should "calculate the length of a list" in {
    def f(a: Int, z: Int): Int = z + 1
    MyList.foldRight(list, 0)(f) shouldBe 3
  }

  "foldLeft" should "sum up" in {
    MyList.foldLeft(list, 0)(_ + _) shouldBe 6
  }

  it should "make a product" in {
    MyList.foldLeft(list, 1)(_ * _) shouldBe 6
  }

  it should "calculate the length of a list" in {
    def f(z: Int, a: Int): Int = z + 1
    MyList.foldLeft(list, 0)(f) shouldBe 3
  }

  it should "reverse a list" in {
    def f(z: MyList[Int], a: Int): MyList[Int] = Cons(a, z)
    MyList.foldLeft(list, MyNil:MyList[Int])(f) shouldBe MyList(3,2,1)
  }

  "append" should "append a list to another" in {
    MyList.append(MyList(1,2,3), MyList(4,5,6)) shouldBe MyList(1,2,3,4,5,6)
  }

  "flatten" should "append a list of lists to one list" in {
    MyList.flatten(MyList(MyList(1,2,3), MyList(4,5,6), MyList(7,8,9))) shouldBe MyList(1,2,3,4,5,6,7,8,9)
  }
}
