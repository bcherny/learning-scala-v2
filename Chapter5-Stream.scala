sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
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

}
case object Empty extends Stream[Nothing] {
  override def equals(b: Any): Boolean = b.isInstanceOf[Empty.type]
  override def hashCode: Int = this.toList.hashCode
  override def toString: String = ""
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def equals(b: Any): Boolean = this.hashCode == b.hashCode
  override def hashCode: Int = this.toList.hashCode
  override def toString: String = h().toString ++ "," ++ t().toString
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
}

object Chapter5 {
  def main(args: Array[String]): Unit = {
    val as = Stream(1, 2, 3)
    eq(as.toList, List(1, 2, 3))
    eq(as.take(2), Stream(1, 2))
    eq(as.drop(2), Stream(3))
    eq(as.takeWhile(_ < 3), Stream(1, 2))
  }

  def eq[A](a: A, b: A) =
    assert(a == b, s"Expected $a to equal $b")
}
