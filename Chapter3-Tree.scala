sealed trait Tree[+A] {
  // 3.25
  def size: Int =
    this match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + l.size + r.size
    }

  // 3.27
  def depth: Int =
    this match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + (l.depth max r.depth)
    }

  // 3.28
  def map[B](f: A => B): Tree[B] =
    this match {
      case Leaf(a)      => Leaf(f(a))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
    }

  // 3.29
  def foldLeft[B](z: B)(f: (B, A) => B): B =
    this match {
      case Leaf(a)      => f(z, a)
      case Branch(l, r) => r.foldLeft(l.foldLeft(z)(f))(f)
    }
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a)      => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }
}

object Chapter3 {
  def main(args: Array[String]): Unit = {
    val t = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
    eq(t.size, 7)
    eq(Tree.maximum(t), 4)
    eq(t.depth, 4)
    eq(t.map(_ * 2), Branch(Leaf(2), Branch(Branch(Leaf(4), Leaf(6)), Leaf(8))))
    eq(t.foldLeft(10)(_ + _), 20)
  }

  def eq[A](a: A, b: A) =
    assert(a == b, s"Expected $a to equal $b")
}
