sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  def flatMap2[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case None => default
    case Some(_) => this
  }
  def orElse2[B >: A](default: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(default)
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => f(a) match {
      case false => None
      case true => this
    }
  }
}
case class Some[+A](a: A) extends Option[A]
case object None extends Option[Nothing]

object Chapter4 {
  def main(args: Array[String]): Unit = {
    val a = Some(3)
    val f = (a: Int) => a * 2

    eq(a.map(f), Some(6))
    eq(None.map(f), None)

    eq(a.flatMap(a => Some(f(a))), Some(6))
    eq(None.flatMap(a => Some(f(a))), None)

    eq(a.flatMap2(a => Some(f(a))), Some(6))
    eq(None.flatMap2(a => Some(f(a))), None)

    eq(a.getOrElse(1), 3)
    eq(None.getOrElse(1.0), 1)

    eq(a.orElse(Some(1)), a)
    eq(None.orElse(Some(1)), Some(1))

    eq(a.orElse2(Some(1)), a)
    eq(None.orElse2(Some(1)), Some(1))

    eq(a.filter(_ > 2), a)
    eq(a.filter(_ > 3), None)
    eq((None: Option[Int]).filter(_ > 2), None)

    eq(mean(Seq()), None)
    eq(mean(Seq(1,2,3)), Some(2))

    eq(variance(Seq()), None)
    eq(variance(Seq(1,1,1)), Some(0))
    eq(variance(Seq(1,2)), Some(.25))

    eq(map2((None: Option[Int]), Some(2))(_ + _), None)
    eq(map2(Some(1), (None: Option[Int]))(_ + _), None)
    eq(map2(Some(1), Some(2))(_ + _), Some(3))

    eq(sequence(List()), Some(List()))
    eq(sequence(List(Some(1), Some(2))), Some(List(1, 2)))
    eq(sequence(List(Some(1), None, Some(2))), None)

    eq(traverse(List[String]())(a => Try(a.toInt)), Some(List()))
    eq(traverse(List("1", "2"))(a => Try(a.toInt)), Some(List(1, 2)))
    eq(traverse(List("1", "a"))(a => Try(a.toInt)), None)
  }

  def Try[A](a: => A): Option[A] =
    try (Some(a))
    catch {case e: Throwable => None}

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs)
      .map(m =>
        xs.map(x => math.pow(x - m, 2))
      )
      .flatMap(mean)
  }
  def mean(xs: Seq[Double]): Option[Double] =
    xs.isEmpty match {
      case false => Some(xs.sum/xs.length)
      case true => None
    }

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List()): Option[List[A]])(
      (c, p) => map2(c, p)(_ :: _)
    )

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map f)

  def eq[A](a: A, b: A) =
    assert(a == b, s"Expected $a to equal $b")
}
