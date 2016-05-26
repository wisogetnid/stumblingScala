package fp

sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this.flatMap(a => MyRight(f(a)))

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyRight(value) => f(value)
    case e => e.asInstanceOf[MyEither[EE, B]]
  }

  def orElse[EE >: E, B >: A](b: => B): MyEither[EE, B] = this match {
    case MyLeft(value) => MyRight(b)
    case right => right
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = this.flatMap(a => b.map(bb => f(a, bb)))
}

object MyEither {
  def sequence[E, A](as: List[MyEither[E, A]]): MyEither[E, List[A]] = as match {
    case Nil => MyRight(Nil)
    case h :: t => h match {
      case MyRight(v) => sequence(t).map(v :: _)
      case MyLeft(e) => MyLeft(e)
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = as match {
    case Nil => MyRight(Nil)
    case h :: t => f(h) match {
      case MyRight(v) => traverse(t)(f).map(v :: _)
      case MyLeft(e) => MyLeft(e)
    }
  }
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]
