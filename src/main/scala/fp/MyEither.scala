package fp

sealed trait MyEither[+E, +A] {
  def map[B](f: A => B) = this.flatMap(a => MyRight(f(a)))

  def flatMap[EE >: E,B](f: A => MyEither[EE, B]) = this match {
    case MyRight(value) => f(value)
    case e => e
  }

}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
}