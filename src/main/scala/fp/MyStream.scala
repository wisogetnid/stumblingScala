package fp

sealed trait MyStream[+A] {
  def takeWhile(f: (A) => Boolean): MyStream[A] = this match {
    case MyEmpty => MyEmpty
    case MyCons(h, t) => if (f(h())) MyCons(h, () => t().takeWhile(f)) else MyEmpty
  }

  def drop(n: Int): MyStream[A] = this match {
    case MyEmpty => MyEmpty
    case MyCons(h, t) => if (n > 0) t().drop(n - 1) else MyCons(h, t)
  }

  def take(n: Int): MyStream[A] = this match {
    case MyEmpty => MyEmpty
    case MyCons(h, t) => if (n <= 0) MyEmpty else MyCons(h, () => t().take(n - 1))
  }

  def toList: List[A] = this match {
    case MyEmpty => List()
    case MyCons(h, t) => h() :: t().toList
  }
}

case object MyEmpty extends MyStream[Nothing]
case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    MyCons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] = {
    if (as.isEmpty) MyEmpty else MyCons(() => as.head, () => apply(as.tail: _*))
  }
}
