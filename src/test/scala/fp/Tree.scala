package fp

sealed trait Tree[A] {
  def depth: Int
  def size: Int
  def maximum(f: (A, A) => A): A
  def map(f: (A) => A): Tree[A]
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = left.size + right.size + 1

  override def maximum(f: (A, A) => A): A =
    f(left.maximum(f), right.maximum(f))

  override def depth: Int = 1 + left.depth + right.depth

  override def map(f: (A) => A): Tree[A] = Branch(left.map(f), right.map(f))
}

case class Leaf[A](value: A) extends Tree[A] {
  override def size: Int = 1

  override def maximum(f: (A, A) => A): A = value

  override def depth: Int = 0

  override def map(f: (A) => A): Tree[A] = Leaf(f(value))
}
