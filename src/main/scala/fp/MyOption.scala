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

object MyOption {
  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((op: A, maybeAccu: Option[List[B]]) => f(op) match {
      case None => None
      case Some(value) => maybeAccu.map( value :: _ )
  })

  def sequence[A](maybeAs: List[MyOption[A]]): MyOption[List[A]] =
    maybeAs.foldRight(MySome(Nil): MyOption[List[A]])((maybeOp: MyOption[A], maybeAccu: MyOption[List[A]]) => maybeOp match {
      case MyNone => MyNone
      case MySome(value) => maybeAccu.map( value :: _ )
    })

  def map2[A,B,C](maybeA: MyOption[A], maybeB: MyOption[B])(f: (A,B) => C): MyOption[C] =
    maybeA.flatMap(a => maybeB.map(b => f(a, b)))
}

case object MyNone extends MyOption[Nothing]

case class MySome[+A](get: A) extends MyOption[A]