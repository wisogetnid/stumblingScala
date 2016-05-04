package fp

import org.scalatest.{FlatSpec, Matchers}

class MyTreeTest extends FlatSpec with Matchers {

  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(5), Leaf(4)), Leaf(3)))

  "size" should "print the number of all branches and leaves" in {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    tree.size shouldBe 5
  }

  "maximum" should "return the maximum value of the tree" in {
    tree.maximum(_ max _) shouldBe 5
  }

  "depth" should "return the mazimum path length from root to any leaf" in {
    tree.depth shouldBe 4
  }

  "map" should "execute a function with every Leaf of a tree" in {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    tree.map(_ + 1) shouldBe Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
  }
}
