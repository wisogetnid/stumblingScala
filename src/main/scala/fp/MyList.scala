package fp

sealed trait MyList[+A] {
  def add[B >: A](b: B): MyList[B]
  def remove[B >: A](b: B): MyList[B]
  def drop(n: Int): MyList[A]
  def init: MyList[A]
}

case object MyNil extends MyList[Nothing] {
  override def add[B >: Nothing](b: B): MyList[B] = Cons(b, MyNil)

  override def remove[B >: Nothing](b: B): MyList[B] = MyNil

  override def drop(n: Int): MyList[Nothing] = MyNil

  override def init: MyList[Nothing] = MyNil
}

case class Cons[A](head: A, tail: MyList[A]) extends MyList[A] {
  override def add[B >: A](b: B): MyList[B] = Cons(b, this)

  override def remove[B >: A](b: B): MyList[B] =
    if (this.head == b)
      this.tail
    else
      Cons(this.head, this.tail.remove(b))

  override def drop(n: Int): MyList[A] =
    if (n == 0)
      this
    else
      this.tail.drop(n - 1)

  override def init: MyList[A] = this match {
    case Cons(head, MyNil) => MyNil
    case Cons(head, tail) => Cons(head, tail.init)
  }
}

object MyList {
  def hasSubsequence[A](as: MyList[A], sub: MyList[A]): Boolean = ???

  def zipWith[A](as: MyList[A], bs: MyList[A])(f: (A, A) => A): MyList[A] = (as, bs) match {
    case (MyNil, MyNil) => MyNil
    case (MyNil, Cons(h, t)) => Cons(h, zipWith(MyNil, t)(f))
    case (Cons(h, t), MyNil) => Cons(h, zipWith(t, MyNil)(f))
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
  }

  def flatMapFilter[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case MyNil => MyNil
    case Cons(h, t) => flatMap(as)(a => if(f(a)) MyList(a) else MyNil)
  }

  def flatMap[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] = as match {
    case MyNil => MyNil
    case Cons(h, t) => MyList.append(f(h), flatMap(t)(f))
  }

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case MyNil => MyNil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  def map[A,B](as: MyList[A])(f: A => B): MyList[B] = as match {
    case MyNil => MyNil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def typeChanger(as: MyList[Double]): MyList[String] = as match {
    case MyNil => MyNil
    case Cons(h, t) => Cons(h.toString, typeChanger(t))
  }

  def oneAdder(as: MyList[Int]): MyList[Int] = as match {
    case MyNil => MyNil
    case Cons(h, t) => Cons(h + 1, oneAdder(t))
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty)
      MyNil
    else
      Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case MyNil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case MyNil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def append[A](as: MyList[A], bs: MyList[A]): MyList[A] = foldRight(as, bs)(Cons(_, _))

  def flatten[A](list: MyList[MyList[A]]): MyList[A] = list match {
    case MyNil => MyNil
    case Cons(h, t) => foldLeft(t, h)(append)
  }
}
