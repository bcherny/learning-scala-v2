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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

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

  // 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, l) => l + 1)

  // 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // 3.11
  def sum2(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)
  def product2(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)
  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((l, _) => l + 1)

  // 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((p, c) => Cons(c, p))

  // 3.13
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // 3.14
  def append[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)(Cons[A])

  // 3.15
  def flatten[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil: List[A])(append)

}

object Chapter3 {

  def main(args: Array[String]): Unit = {
    val as = List.apply(1, 2, 3)
    val bs = List.apply(1.0, 2.0, 3.0)
    val cs = List.apply(as, as, as)
    println(s"tail($as) = " + List.tail(as))
    println("tail(Nil) = " + List.tail(Nil))
    println(s"setHead($as, 4) = " + List.setHead(as, 4))
    println(s"drop($as, 0) = " + List.drop(as, 0))
    println(s"drop($as, 2) = " + List.drop(as, 2))
    println(s"drop($as, 5) = " + List.drop(as, 5))
    println(s"dropWhile($as, _ < 3) = " + List.dropWhile[Int](as, _ < 3))
    println(s"dropWhile($as, _ < 10) = " + List.dropWhile[Int](as, _ < 10))
    println(s"init($as) = " + List.init(as))
    println("init(Nil) = " + List.init(Nil))
    println(s"foldRight($as, 0)(_ - _) = " + List.foldRight(as, 0)(_ - _))
    println(s"foldRight($as, Nil)(Cons(_, _)) = " + List.foldRight(as, Nil: List[Int])(Cons(_, _)))
    println(s"length($as) = " + List.length(as))
    println(s"foldLeft($as, 0)(_ + _) = " + List.foldLeft(as, 0)(_ + _))
    println(s"foldLeft($as, 0)(_ - _) = " + List.foldLeft(as, 0)(_ - _))
    println(s"sum2($as) = " + List.sum2(as))
    println(s"product2($bs) = " + List.product2(bs))
    println(s"length2($as) = " + List.length2(as))
    println(s"reverse($as) = " + List.reverse(as))
    println(s"foldLeft2($as, 0)(_ - _) = " + List.foldLeft2(as, 0)(_ - _))
    println(s"foldRight2($as, 0)(_ - _) = " + List.foldRight2(as, 0)(_ - _))
    println(s"append($as, $as) = " + List.append(as, as))
    println(s"flatten($cs) = " + List.flatten(cs))
  }

  // 3.1
  // The result is 3.

  // 3.7
  // No, foldRight has to traverse the whole list.

  // 3.8
  // The result is List(1, 2, 3), same as the input! This is because
  // List forms a monoid with foldRight: Nil is the empty monoid, and
  // Cons is the monoid-appending operation.
}