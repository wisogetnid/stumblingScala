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
    case MyCons(h, t) => if (n <= 0) empty else cons(h(), t().take(n - 1))
  }

  def toList: List[A] = this match {
    case MyEmpty => List()
    case MyCons(h, t) => h() :: t().toList
  }

  def uMap[B](f: A => B): MyStream[B] = MyStream.unfold(this) {
    case MyCons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def uTake(n: Int): MyStream[A] = MyStream.unfold((n, this)) {
    case (n:Int, MyCons(h, t)) if n > 0 => Some(h(), (n-1, t()))
    case _ => None
  }

  def uTakeWhile(f: A => Boolean): MyStream[A] = MyStream.unfold(this) {
    case MyCons(h, t) if f(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](bs: MyStream[B])(f: (A, B) => C): MyStream[C] = MyStream.unfold((this, bs)) {
    case (MyCons(ha, ta), MyCons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
    case _ => None
  }

  def zipAll[B](bs: MyStream[B]): MyStream[(Option[A], Option[B])] = MyStream.unfold((this, bs)) {
    case (MyCons(ha, ta), MyCons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    case (MyCons(h, t), MyEmpty) => Some((Some(h()), None), (t(), empty))
    case (MyEmpty, MyCons(h, t)) => Some((None, Some(h())), (empty, t()))
    case _ => None
  }
}

case object MyEmpty extends MyStream[Nothing]

case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty[A]
    }

  def fibonacci: MyStream[Int] = {
    def go(i: Int, j: Int): MyStream[Int] = cons(i, go(j, i+j))
    go(0, 1)
  }

//  def fibAdder(a: Int, s: Int) = Some((a, (s, a + s)))
//  def uFibonacci: MyStream[Int] = unfold[Int, (Int, Int)]((0,1)){ fibAdder }
  def uFibonacci: MyStream[Int] = unfold((0,1)){ x => Some(x._1, (x._2, x._1 + x._2)) }

  def from(n: Int): MyStream[Int] = cons(n, from(n + 1))

  def uFrom(n: Int): MyStream[Int] = unfold(n)(s => Some(s, s + 1))

  def uConstant[A](a: A): MyStream[A] = unfold(a)(s => Some(a, a))

  def constant[A](a: A): MyStream[A] = cons(a, constant(a))

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
