package fp

import fp.MyStream.{cons, empty}

sealed trait MyStream[+A] {
  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    this.foldRight(empty[B]){ (a, b) => f(a).append(b) }

  def append[AA >: A](as: => MyStream[AA]): MyStream[AA] =
    this.foldRight(as){ (a, b) => cons(a, b)}

  def filter(p: A => Boolean): MyStream[A] =
    this.foldRight(empty[A]) { (a, b) => if (p(a)) cons(a, b) else b}

  def map[B](f: A => B): MyStream[B] =
    this.foldRight(empty[B]) { (a, b) => cons(f(a), b) }

  def headOption: Option[A] =
    this.foldRight(None: Option[A]) { (a, b) => Some(a) }

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case MyCons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case MyCons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case MyEmpty => MyEmpty
    case MyCons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else MyEmpty
  }

  def drop(n: Int): MyStream[A] = this match {
    case MyEmpty => MyEmpty
    case MyCons(h, t) => if (n > 0) t().drop(n - 1) else cons(h(), t())
  }

  def take(n: Int): MyStream[A] = this match {
    case MyEmpty => MyEmpty
    case MyCons(h, t) => if (n <= 0) MyEmpty else cons(h(), t().take(n - 1))
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
    if (as.isEmpty) MyEmpty else cons(as.head, apply(as.tail: _*))
  }
}
