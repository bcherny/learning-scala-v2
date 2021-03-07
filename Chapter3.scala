sealed trait List[+A] {
  override def toString(): String = {
    def toString(as: List[A]): String =
      as match {
        case Nil => ""
        case Cons(h, Nil) => h.toString
        case Cons(h, t) => h.toString + "," + toString(t)
      }
    "[" + toString(this) + "]"
  }
}
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  // 3.2
  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil // Alternatively, this could throw a runtime exception
      case Cons(_, a) => a
    }

  // 3.3
  def setHead[A](as: List[A], a: A): List[A] =
    Cons(a, tail(as))

  // 3.4
  def drop[A](as: List[A], n: Int): List[A] =
    if (n < 1) as
    else List.drop(List.tail(as), n - 1)

  // 3.5
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(a, t) => f(a) match {
        case false => as
        case true => dropWhile(t, f)
      }
    }

  // 3.6
  def init[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(a, Cons(b, Nil)) => Cons(a, Nil)
      case Cons(h, t) => Cons(h, init(t))
    }
}

object Chapter3 {

  def main(args: Array[String]): Unit = {
    val as = List.apply(1, 2, 3)
    println(s"tail($as) = " + List.tail(as))
    println("tail(Nil) = " + List.tail(Nil))
    println(s"setHead($as, 4) = " + List.setHead(as, 4))
    println(s"drop($as, 0) = " + List.drop(as, 0))
    println(s"drop($as, 2) = " + List.drop(as, 2))
    println(s"drop($as, 5) = " + List.drop(as, 5))
    println(s"dropWhile($as, _ < 3) = " + List.dropWhile[Int](as, _ < 3))
    println(s"dropWhile($as, _ < 10) = " + List.dropWhile[Int](as, _ < 10))
    println(s"init($as) = " + List.init(as))
    println(s"init(Nil) = " + List.init(Nil))
  }

  // 3.1
  // Result is 3
}