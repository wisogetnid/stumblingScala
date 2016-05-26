package fp

sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this.flatMap(a => MyRight(f(a)))

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyRight(value) => f(value)
    case MyLeft(e) => MyLeft(e)
  }

  def orElse[EE >: E, B >: A](b: => B): MyEither[EE, B] = this match {
    case MyLeft(value) => MyRight(b)
    case right => right
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = this.flatMap(a => b.map(bb => f(a, bb)))
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
}
