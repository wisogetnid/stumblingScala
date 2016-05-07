package fp

sealed trait Tree[A] {
  def depth: Int
  def size: Int
  def maximum(f: (A, A) => A): A
  def map(f: (A) => A): Tree[A]
  def fold[B](f: A => B)(g: (B, B) => B): B
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = 1 + left.size + right.size

  override def depth: Int = 1 + left.depth + right.depth

  override def maximum(f: (A, A) => A): A = f(left.maximum(f), right.maximum(f))

  override def map(f: (A) => A): Tree[A] = Branch(left.map(f), right.map(f))

  override def fold[B](f: A => B)(g: (B, B) => B): B = g(left.fold(f)(g), right.fold(f)(g))
}

case class Leaf[A](value: A) extends Tree[A] {
  override def size: Int = 1

  override def depth: Int = 0

  override def maximum(f: (A, A) => A): A = value

  override def map(f: (A) => A): Tree[A] = Leaf(f(value))

  override def fold[B](f: A => B)(g: (B, B) => B): B = f(value)
}
