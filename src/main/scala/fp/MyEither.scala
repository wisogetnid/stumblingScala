package fp

sealed trait MyEither[+E, +A] {
  def map[B](f: A => B) = this match {
    case MyRight(value) => MyRight(f(value))
    case e => e
  }
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A]