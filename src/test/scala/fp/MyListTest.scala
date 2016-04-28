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

  "oneAdder" should "add 1 to each value of a list" in {
    MyList.oneAdder(list) shouldBe MyList(2,3,4)
  }

  "typeChanger" should "change type from Double into String" in {
    MyList.typeChanger(MyList(1.0, 2.0, 3.0)) shouldBe MyList("1.0", "2.0", "3.0")
  }

  "map" should "provide easy way to implement oneAdder" in {
    MyList.map(list)(_ + 1) shouldBe MyList(2,3,4)
  }

  it should "provide easy way to implement typeChanger" in {
    MyList.map(MyList(1.0, 2.0, 3.0))(_.toString) shouldBe MyList("1.0", "2.0", "3.0")
  }

  "filter" should "remove items not matching a predicate" in {
    MyList.filter(list)(_ % 2 == 1) shouldBe MyList(1,3)
  }

  "flatMap" should "execute a function returning a list over every element of a list" in {
    MyList.flatMap(list)(e => MyList(e, e)) shouldBe MyList(1,1,2,2,3,3)
  }

  "flatMapFilter" should "do the same as filter(remove items not matching a predicate)" in {
    MyList.flatMapFilter(list)(_ % 2 == 1) shouldBe MyList(1,3)
  }

  "zipWith" should "merge two lists using a function" in {
    MyList.zipWith(list, list)(_ + _) shouldBe MyList(2,4,6)
  }

  it should "merge two different sized lists using a function" in {
    MyList.zipWith(list, MyList(3))(_ + _) shouldBe MyList(4,2,3)
    MyList.zipWith(list, MyList(3, 3, 3, 3))(_ + _) shouldBe MyList(4, 5, 6, 3)
  }

  "hasSubsequence" should "find subsequences within a sequence" in {
    MyList.hasSubsequence(list, MyList(2)) shouldBe true
  }

  it should "find complex subsequences" in {
    MyList.hasSubsequence(MyList(1,3,1,2,3,2,3), list) shouldBe true
  }
  
  it should "return false if the subsequence could not be found" in {
    MyList.hasSubsequence(list, MyList(4)) shouldBe false
  }
}
