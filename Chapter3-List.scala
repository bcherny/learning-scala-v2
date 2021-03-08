sealed trait List[+A] {
  override def toString(): String = {
    def toString(as: List[A]): String =
      as match {
        case Nil          => ""
        case Cons(h, Nil) => h.toString
        case Cons(h, t)   => h.toString + "," + toString(t)
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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil        => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  // 3.2
  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil        => Nil // Alternatively, this could throw a runtime exception
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
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(a, t) =>
        f(a) match {
          case false => as
          case true  => dropWhile(t)(f)
        }
    }

  // 3.6
  def init[A](as: List[A]): List[A] =
    as match {
      case Nil                   => Nil
      case Cons(a, Cons(b, Nil)) => Cons(a, Nil)
      case Cons(h, t)            => Cons(h, init(t))
    }

  // 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, l) => l + 1)

  // 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil        => z
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

  // 3.16
  def add1(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((c, p) => Cons(c + 1, p))

  // 3.17
  def mapToString(as: List[Double]): List[String] =
    map(as)(_.toString)

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil        => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  def map2[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((c, p) => Cons(f(c), p))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((c, p) =>
      if (f(c)) Cons((c), p)
      else p
    )

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  // 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // 3.22
  def zipSum(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, zipSum(ta, tb))
    }

  // 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }

  // 3.24
  @annotation.tailrec
  def hasSubsequence[A](as: List[A], needle: List[A]): Boolean =
    (as, needle) match {
      case (_, Nil) => true
      case (Cons(ah, at), Cons(nh, nt)) =>
        hasSubsequence(
          at,
          if (ah == nh) nt else needle
        )
      case _ => false
    }
}

object Chapter3 {

  def main(args: Array[String]): Unit = {
    val as = List(1, 2, 3)
    val bs = List(1.0, 2.0, 3.0)
    val cs = List(as, as, as)
    eq(List.tail(as), List(2, 3))
    eq(List.tail(Nil), Nil)
    eq(List.setHead(as, 4), List(4, 2, 3))
    eq(List.drop(as, 0), as)
    eq(List.drop(as, 2), Cons(3, Nil))
    eq(List.drop(as, 5), Nil)
    eq(List.dropWhile(as)(_ < 3), List(3))
    eq(List.dropWhile(as)(_ < 10), Nil)
    eq(List.init(Nil), Nil)
    eq(List.init(as), List(1, 2))
    eq(List.foldRight(as, 0)(_ - _), 2)
    eq(List.foldRight(as, Nil: List[Int])(Cons(_, _)), List(1, 2, 3))
    eq(List.length(Nil), 0)
    eq(List.length(as), 3)
    eq(List.foldLeft(as, 0)(_ + _), 6)
    eq(List.foldLeft(as, 0)(_ - _), -6)
    eq(List.sum2(as), 6)
    eq(List.product2(bs), 6.0)
    eq(List.length2(as), 3)
    eq(List.reverse(as), List(3, 2, 1))
    eq(List.foldLeft2(as, 0)(_ - _), -6)
    eq(List.foldRight2(as, 0)(_ - _), 2)
    eq(List.append(as, as), List(1, 2, 3, 1, 2, 3))
    eq(List.flatten(cs), List(1, 2, 3, 1, 2, 3, 1, 2, 3))
    eq(List.add1(as), List(2, 3, 4))
    eq(List.mapToString(bs), List("1.0", "2.0", "3.0"))
    eq(List.map(as)(_ + 1), List(2, 3, 4))
    eq(List.map2(as)(_ + 1), List(2, 3, 4))
    eq(List.filter(as)(_ < 3), List(1, 2))
    eq(List.flatMap(as)(_ => as), List(1, 2, 3, 1, 2, 3, 1, 2, 3))
    eq(List.filter2(as)(_ < 3), List(1, 2))
    eq(List.zipSum(as, as), List(2, 4, 6))
    eq(List.zipWith(as, bs)(_ + _), List(2, 4, 6))
    // eq(List.hasSubsequence(as, Nil), true)
    eq(List.hasSubsequence(as, List(1, 2)), true)
    eq(List.hasSubsequence(as, List(1, 2, 3)), true)
    eq(List.hasSubsequence(as, List(2, 3)), true)
    eq(List.hasSubsequence(as, List(1, 2, 4)), false)
  }

  // 3.1
  // The result is 3.

  // 3.7
  // No, foldRight has to traverse the whole list.

  // 3.8
  // The result is List(1, 2, 3), same as the input! This is because
  // List forms a monoid with foldRight: Nil is the empty monoid, and
  // Cons is the monoid-appending operation.

  def eq[A](a: A, b: A) =
    assert(a == b, s"Expected $a to equal $b")
}
