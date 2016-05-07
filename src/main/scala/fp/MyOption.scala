package fp

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this.flatMap(a => MySome(f(a)))

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(value) => f(value)
  }

  def getOrElse[B >: A](default: B): B = this match {
    case MyNone => default
    case MySome(value) => value
  }

  def orElse[B >: A](ob: => B): MyOption[B] = MySome(this.getOrElse(ob))

  def filter(predicate: A => Boolean): MyOption[A] = this.flatMap(a => if (predicate(a)) MySome(a) else MyNone)
}

case object MyNone extends MyOption[Nothing]

case class MySome[+A](get: A) extends MyOption[A]