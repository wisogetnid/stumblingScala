package fp

import org.scalatest.{FlatSpec, Matchers}

class MyTreeTest extends FlatSpec with Matchers {

  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(5), Leaf(4)), Leaf(3)))

  "size" should "print the number of all branches and leaves" in {
    tree.size shouldBe 9
  }

  "depth" should "return the mazimum path length from root to any leaf" in {
    tree.depth shouldBe 4
  }

  "maximum" should "return the maximum value of the tree" in {
    tree.maximum(_ max _) shouldBe 5
  }

  "map" should "execute a function with every Leaf of a tree" in {
    tree.map(_ + 1) shouldBe Branch(Branch(Leaf(2), Leaf(3)), Branch(Branch(Leaf(6), Leaf(5)), Leaf(4)))
  }

  "fold" should "be open to calculate size" in {
    tree.fold(_ => 1)(_ + _ + 1) shouldBe 9
  }

  it should "be open to calculate depth" in {
    tree.fold(_ => 0)(_ + _ + 1) shouldBe 4
  }

  it should "be open to calculate max" in {
    tree.fold(identity)((a, b) => if (a > b) a else b) shouldBe 5
  }

  it should "be open to implement map" in {
//    tree.map(_ + 1)
    tree.fold(a => Leaf(a + 1): Tree[Int])(Branch(_, _)) shouldBe Branch(Branch(Leaf(2), Leaf(3)), Branch(Branch(Leaf(6), Leaf(5)), Leaf(4)))
  }
}
