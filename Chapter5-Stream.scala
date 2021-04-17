sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // 5.1
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  // 5.2
  def take(n: Int): Stream[A] =
    if (n < 1) Stream.empty
    else this match {
      case Empty => Stream.empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }

  def drop(n: Int): Stream[A] =
    if (n < 1) this
    else this match {
      case Empty => Stream.empty
      case Cons(h, t) => t().drop(n - 1)
    }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Stream.empty
    case Cons(h, t) =>
      if (p(h())) Cons(h, () => t().takeWhile(p))
      else Stream.empty
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) =>
      if (p(a)) Stream.cons(a, b)
      else Empty
    )

  // 5.6
  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) =>
      if (f(a)) Stream.cons(a, b)
      else b
    )

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)(Stream.cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])(f(_).append(_))
}
case object Empty extends Stream[Nothing] {
  override def equals(b: Any): Boolean = b.isInstanceOf[Empty.type]
  override def hashCode: Int = this.toList.hashCode
  override def toString: String = ""
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def equals(b: Any): Boolean = this.hashCode == b.hashCode
  override def hashCode: Int = this.toList.hashCode
  override def toString: String = this.toList.toString
}

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // 5.8
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  // 5.11
  def unfold[A, S](z: S)(d: S => Option[(A, S)]): Stream[A] = d(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(d))
    case None => empty
  }
}

object Chapter5 {
  def main(args: Array[String]): Unit = {
    val as = Stream(1, 2, 3)
    eq(as.toList, List(1, 2, 3))
    eq(as.take(2), Stream(1, 2))
    eq(as.drop(2), Stream(3))
    eq(as.takeWhile(_ < 3), Stream(1, 2))
    eq(as.forAll(_ < 3), false)
    eq(as.forAll(_ < 4), true)
    eq(as.takeWhile2(_ < 3), Stream(1, 2))
    eq(as.headOption2, Some(1))
    eq(Empty.headOption2, None)
    eq(as.map(_ * 2), Stream(2, 4, 6))
    eq(as.filter(_ < 3), Stream(1, 2))
    eq(as.append(Stream(4)), Stream(1, 2, 3, 4))
    eq(as.flatMap(a => Stream(a * 2)), Stream(2, 4, 6))
    eq(Stream.constant(1).take(4), Stream(1, 1, 1, 1))
    eq(Stream.from(1000).take(4), Stream(1000, 1001, 1002, 1003))
    eq(fibs.take(10), Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    eq(Stream.unfold(5)(s => Some((s, s * 2))).take(3), Stream(5, 10, 20))
  }

  // 5.10
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, go(b, a + b))
    go(0, 1)
  }

  def eq[A](a: A, b: A) =
    assert(a == b, s"Expected $a to equal $b")
}
